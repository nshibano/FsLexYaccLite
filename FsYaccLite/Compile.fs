module FsLexYacc.FsYacc.Compile

open System
open System.Collections.Generic
open System.IO
open System.Diagnostics

open Printf

open Syntax
open Preprocess

type Action = 
  | Shift of stateIndex : int
  | Reduce of productionIndex : int
  | Accept
  | Error
    
type TerminalIndex = int
type NonTerminalIndex = int
type ProductionIndex = int

type SymbolIndex =
    | TerminalIndex of int : TerminalIndex
    | NonTerminalIndex of int : NonTerminalIndex

type LR0Item =
    { ProductionIndex : ProductionIndex
      DotIndex : int }

let createLR0Item productionIndex = { ProductionIndex = productionIndex; DotIndex = 0 }

type KernelItemIndex =
    { KernelIndex : int
      Item : LR0Item }

type GotoItemIndex =
    { KernelIndex : int
      SymbolIndex : SymbolIndex }

type LR1Item =
    { LR0Item : LR0Item
      Lookahead : TerminalIndex }

let memoize1 f =
    let d = Dictionary(HashIdentity.Structural)
    fun x ->
        match d.TryGetValue(x) with
        | true, y -> y
        | false, _ ->
            let y = f x
            d.[x] <- y
            y

let memoize2 f =
    let d = Dictionary(HashIdentity.Structural)
    fun x y ->
        match d.TryGetValue((x, y)) with
        | true, z -> z
        | false, _ ->
            let z = f x y
            d.[(x, y)] <- z
            z

type MultiDictionary<'T, 'U> = Dictionary<'T, HashSet<'U>>

let MultiDictionary_Create<'T, 'U when 'T : equality>() : MultiDictionary<'T, 'U> = Dictionary<'T, HashSet<'U>>(HashIdentity.Structural)

let MultiDictionary_Contains (d : MultiDictionary<'T, 'U>) (k : 'T) (v : 'U) =
    match d.TryGetValue(k) with
    | true, values -> values.Contains(v)
    | false, _ -> false

let MultiDictionary_Add (d : MultiDictionary<'T, 'U>) (k : 'T) (v : 'U) =
    match d.TryGetValue(k) with
    | true, values -> values.Add(v) |> ignore
    | false, _ ->
        let values = HashSet(HashIdentity.Structural)
        values.Add(v) |> ignore
        d.[k] <- values

let sortedArrayofList (l : List<'T>) =
    let ary = l.ToArray()
    Array.sortInPlace ary
    ary

let sortedArrayofHashSet (hs : HashSet<'T>) =
    let ary = Array.zeroCreate hs.Count
    hs.CopyTo(ary)
    Array.sortInPlace ary
    ary

type CompiledTable =
    {
        Productions : (string * NonTerminalIndex * Symbol array * Code option) []
        States : ProductionIndex [] [] 
        StartStates : int []
        ActionTable : ((Associativity * int) option * Action) [] []
        GotoTable : int option [] [] 
        EndOfInputTerminalIndex : int
        ErrorTerminalIndex : int
    }

let compile (logf : System.IO.TextWriter option) (newprec:bool) (norec:bool) (spec : ProcessedParserSpec) =
    let stopWatch = Stopwatch.StartNew()
    let reportTime() =
        printfn "time: %d(ms)" stopWatch.ElapsedMilliseconds
        stopWatch.Restart()

    let indexOfNonTerminal =
        let d = Dictionary<string, NonTerminalIndex>()
        for i = 0 to spec.NonTerminals.Length - 1 do
            d.Add(spec.NonTerminals.[i], i)
        d

    let indexOfTerminal =
        let d = Dictionary<string, int>()
        for i = 0 to spec.Terminals.Length - 1 do
            d.Add(fst spec.Terminals.[i], i)
        d

    let indexOfSymbol (symbol : Symbol) =
        match symbol with
        | Terminal s -> TerminalIndex (indexOfTerminal.[s])
        | NonTerminal s -> NonTerminalIndex (indexOfNonTerminal.[s])

    let productionHeads = Array.map (fun p -> indexOfNonTerminal.[p.Head]) spec.Productions
    let productionBodies = Array.map (fun p -> Array.map indexOfSymbol p.Body) spec.Productions
    let productionPrecedences = Array.map (fun p -> p.PrecedenceInfo) spec.Productions
    let productionsOfNonTerminal : ProductionIndex[][] =
        let table = Array.init spec.NonTerminals.Length (fun _ -> ResizeArray<int>())
        for i = 0 to spec.Productions.Length - 1 do
            table.[productionHeads.[i]].Add(i)
        Array.map (fun (l : ResizeArray<int>) -> l.ToArray()) table

    let dummyLookaheadIdx = indexOfTerminal.[dummyLookahead]
    let endOfInputTerminalIdx = indexOfTerminal.[endOfInputTerminal]
    let errorTerminalIdx = indexOfTerminal.[errorTerminal]

    // Compute the FIRST function
    printf  "computing first function..."; stdout.Flush();

    let firstSetOfSymbol =
        let accu = Dictionary<SymbolIndex, HashSet<TerminalIndex option>>(HashIdentity.Structural)

        // For terminals, add itself (Some term) to its first-set.
        for term = 0 to spec.Terminals.Length - 1 do
            let set = HashSet(HashIdentity.Structural)
            set.Add(Some term) |> ignore
            accu.Add(TerminalIndex term, set)

        // For non-terminals, start with empty set.
        for nonTerm = 0 to spec.NonTerminals.Length - 1 do
            accu.Add(NonTerminalIndex nonTerm, HashSet(HashIdentity.Structural))
        
        let mutable added = false
        let add symbolIndex firstSetItem =
            added <- accu.[symbolIndex].Add(firstSetItem) || added

        let scan() =
            for prodIdx = 0 to spec.Productions.Length - 1 do
                let head = productionHeads.[prodIdx]
                let body = productionBodies.[prodIdx]
                let mutable pos = 0
                while pos < body.Length do
                    // add first symbols of production body to the first-set of this production head 
                    for firstSetItem in accu.[body.[pos]] do
                        if firstSetItem.IsSome then
                            add (NonTerminalIndex head) firstSetItem
                    if accu.[body.[pos]].Contains None then
                        // the symbol at pos can be empty, therefore go through the following symbols
                        pos <- pos + 1
                    else
                        // otherwise, stop here
                        pos <- Int32.MaxValue
                if pos = body.Length then
                    // the scan for production body symbols were gone through the end of the body
                    // therefore all symbols in production body can be empty
                    // therefore this production is nullable
                    add (NonTerminalIndex head) None
                
        scan()
        while added do
            added <- false
            scan()

        accu

    let firstSetOfSymbolString (str : SymbolIndex []) (term : TerminalIndex) =
        let accu = HashSet<TerminalIndex>(HashIdentity.Structural)

        let rec loop pos =
            if pos < str.Length then
                let firstSetOfSym = firstSetOfSymbol.[str.[pos]]
                for first in firstSetOfSym do
                    match first with
                    | Some v -> accu.Add(v) |> ignore
                    | None -> ()
                if firstSetOfSym.Contains(None) then
                    loop (pos + 1)
            else
                accu.Add(term) |> ignore

        loop 0
        accu
    
    let firstSetOfSymbolString = memoize2 firstSetOfSymbolString

    let advanceOfItem (item : LR0Item) = { item with DotIndex = item.DotIndex + 1 }
    let isStartItem (item : LR0Item) = Array.contains (spec.NonTerminals.[productionHeads.[item.ProductionIndex]]) spec.StartSymbols
    let isStartItem1 (item : LR1Item) = Array.contains (spec.NonTerminals.[productionHeads.[item.LR0Item.ProductionIndex]]) spec.StartSymbols

    let computeClosure (itemSet : LR0Item []) =
        let mutable accu = HashSet(HashIdentity.Structural)
        for item in itemSet do
            accu.Add(item) |> ignore

        let queue = Queue<LR0Item>(itemSet)
        while queue.Count > 0 do
            let item = queue.Dequeue()
            let body = productionBodies.[item.ProductionIndex]
            if item.DotIndex < body.Length then
                match body.[item.DotIndex] with
                | NonTerminalIndex ni ->
                    for prod in productionsOfNonTerminal.[ni] do
                        let newItem = createLR0Item prod
                        if accu.Add(newItem) then
                            queue.Enqueue newItem
                | _ -> ()

        sortedArrayofHashSet accu

    let computeClosure = memoize1 computeClosure

    // Goto set of a kernel of LR(0) nonTerminals, items etc 
    // Input is kernel, output is kernel
    let computeGotoOfKernel (kernel : LR0Item []) (symbol : SymbolIndex) : LR0Item [] = 
        let accu = List()

        for item in computeClosure kernel do
            let body = productionBodies.[item.ProductionIndex]
            if item.DotIndex < body.Length && body.[item.DotIndex] = symbol then
                accu.Add(advanceOfItem item)

        sortedArrayofList accu

    let computeGotosOfKernel (kernel : LR0Item []) : LR0Item [] [] = 
        let accu = MultiDictionary_Create<SymbolIndex, LR0Item>()

        for item in computeClosure kernel do
            let body = productionBodies.[item.ProductionIndex]
            if item.DotIndex < body.Length then
                MultiDictionary_Add accu body.[item.DotIndex] (advanceOfItem item)
        
        let gotoSymbols = Array.sort (Array.ofSeq accu.Keys)
        Array.map (fun sym -> sortedArrayofHashSet accu.[sym]) gotoSymbols

    // Build the full set of LR(0) kernels 
    reportTime(); printf "building kernels..."; stdout.Flush();
    let startItems = Array.map (fun startSymbol -> createLR0Item productionsOfNonTerminal.[indexOfNonTerminal.[startSymbol]].[0]) spec.StartSymbols
    let startKernels = Array.map (fun x -> [| x |]) startItems
    let kernels = 
        let queue = Queue(startKernels)
        let accu = HashSet(HashIdentity.Structural)
        while queue.Count > 0 do
            let kernel = queue.Dequeue()
            if accu.Add(kernel) then
                for goto in computeGotosOfKernel kernel do
                    queue.Enqueue(goto)
        Array.ofSeq accu
    
    reportTime(); printf "building kernel table..."; stdout.Flush();

    // Give an index to each LR(0) kernel, and from now on refer to them only by index
    let indexOfKernel =
        let d = Dictionary(HashIdentity.Structural)
        for i = 0 to kernels.Length - 1 do
            d.Add(kernels.[i], i)
        d
    
    let startKernelIdxs = Array.map (fun kernel -> indexOfKernel.[kernel]) startKernels
    let startKernelItemIdxs = Array.map2 (fun kernel item -> { KernelIndex = kernel; Item = item }) startKernelIdxs startItems

    /// A cached version of the "goto" computation on LR(0) kernels 
    let gotoKernel (gotoItemIndex : GotoItemIndex) = 
        let gset = computeGotoOfKernel (kernels.[gotoItemIndex.KernelIndex]) gotoItemIndex.SymbolIndex
        if gset.Length = 0 then None else Some (indexOfKernel.[gset])

    let gotoKernel = memoize1 gotoKernel

    // This is used to compute the closure of an LALR(1) kernel 
    //
    // For each item [A --> X.BY, a] in I
    //   For each production B -> g in G'
    //     For each terminal b in FIRST(Ya)
    //        such that [B --> .g, b] is not in I do
    //            add [B --> .g, b] to I
    
    let ComputeClosure1 (iset : LR1Item []) = 
        let acc = HashSet<LR1Item>(HashIdentity.Structural)
        let queue = Queue(iset)
        while queue.Count > 0 do
            let item = queue.Dequeue()
            if acc.Add(item) then
                let body = productionBodies.[item.LR0Item.ProductionIndex]
                if item.LR0Item.DotIndex < body.Length then
                    match body.[item.LR0Item.DotIndex] with
                    | NonTerminalIndex ntB -> 
                        let firstSet = firstSetOfSymbolString body.[(item.LR0Item.DotIndex + 1)..] item.Lookahead
                        for prodIdx in productionsOfNonTerminal.[ntB] do
                            for first in firstSet do
                                queue.Enqueue({ LR0Item = createLR0Item prodIdx; Lookahead = first})
                    | TerminalIndex _ -> ()

        sortedArrayofHashSet acc

    // Compute the "spontaneous" and "propagate" maps for each LR(0) kernelItem 
    //
    // Input: The kernal K of a set of LR(0) items I and a grammar symbol X
    //
    // Output: The lookaheads generated spontaneously by items in I for kernel items 
    // in goto(I,X) and the items I from which lookaheads are propagated to kernel
    // items in goto(I,X)
    //
    // Method
    //   1. Construct LR(0) kernel items (done - above)
    //   2. 
    // TODO: this is very, very slow. 
    //
    // PLAN TO OPTIMIZE THIS;
    //   - Clarify and comment what's going on here
    //   - verify if we really have to do these enormouos closure computations
    //   - assess if it's possible to use the symbol we're looking for to help trim the jset
    
    reportTime(); printf "computing lookahead relations..."; stdout.Flush();

    let closure1OfItemWithDummy (item : LR0Item) = ComputeClosure1 [| { LR0Item = item; Lookahead = dummyLookaheadIdx } |]
    let closure1OfItemWithDummy = memoize1 closure1OfItemWithDummy
        
    let spontaneous, propagate  =

        let spontaneous = HashSet<KernelItemIndex * TerminalIndex>(HashIdentity.Structural)
        let propagate = MultiDictionary_Create<KernelItemIndex, KernelItemIndex>()

        for kernelIdx = 0 to kernels.Length - 1 do
            printf  "."; stdout.Flush();
            for lr0Item in kernels.[kernelIdx] do  
                for lr1Item in closure1OfItemWithDummy lr0Item do
                    let body = productionBodies.[lr1Item.LR0Item.ProductionIndex]
                    if lr1Item.LR0Item.DotIndex < body.Length then
                        match gotoKernel { KernelIndex = kernelIdx; SymbolIndex = body.[lr1Item.LR0Item.DotIndex] } with 
                        | None -> ()
                        | Some gotoKernelIdx ->
                            let gotoItemIdx = { KernelIndex = gotoKernelIdx; Item = advanceOfItem lr1Item.LR0Item }
                            let lookaheadToken = lr1Item.Lookahead
                            if lookaheadToken = dummyLookaheadIdx then
                                MultiDictionary_Add propagate { KernelIndex = kernelIdx; Item = lr0Item } gotoItemIdx |> ignore
                            else
                                spontaneous.Add(gotoItemIdx, lookaheadToken) |> ignore

        spontaneous, propagate
   
    // Repeatedly use the "spontaneous" and "propagate" maps to build the full set 
    // of lookaheads for each LR(0) kernelItem.   
    reportTime(); printf  "building lookahead table..."; stdout.Flush();
    let lookaheadTable = 
        let queue = Queue()

        // Seed the table with the startKernelItems and the spontaneous info
        for idx in startKernelItemIdxs do
            queue.Enqueue(idx,endOfInputTerminalIdx)
        for s in spontaneous do
            queue.Enqueue(s)

        let acc = MultiDictionary_Create<KernelItemIndex, TerminalIndex>()

        // Compute the closure
        while queue.Count > 0 do
            let kernelItemIdx, lookahead = queue.Dequeue()
            MultiDictionary_Add acc kernelItemIdx lookahead
            match propagate.TryGetValue(kernelItemIdx) with
            | true, s ->
                for gotoKernelIdx in s do
                    if not (MultiDictionary_Contains acc gotoKernelIdx lookahead) then 
                        queue.Enqueue(gotoKernelIdx, lookahead)
            |  false, _ -> ()
        
        acc

    let stringOfSym (symbolIndex : SymbolIndex) =
        match symbolIndex with
        | TerminalIndex i -> fst spec.Terminals.[i]
        | NonTerminalIndex i -> spec.NonTerminals.[i]
    
    let stringOfSyms syms = String.Join(" ", Array.map stringOfSym syms)

    reportTime(); printf "building action table..."; stdout.Flush();
    let shiftReduceConflicts = ref 0
    let reduceReduceConflicts = ref 0
    let actionTable = 

        // Now build the action tables. First a utility to merge the given action  
        // into the table, taking into account precedences etc. and reporting errors. 
        let addResolvingPrecedence (arr: _[]) kernelIdx termIdx (precNew, actionNew) = 
            // printf "DEBUG: state %d: adding action for %s, precNew = %a, actionNew = %a\n" kernelIdx (termTab.OfIndex termIdx) outputPrec precNew OutputAction actionNew; 
            // We add in order of precedence - however the precedences may be the same, and we give warnings when rpecedence resolution is based on implicit file orderings 

            let (precSoFar, actionSoFar) as itemSoFar = arr.[termIdx]

            // printf "DEBUG: state %d: adding action for %s, precNew = %a, precSoFar = %a, actionSoFar = %a\n" kernelIdx (termTab.OfIndex termIdx) outputPrec precNew outputPrec precSoFar OutputAction actionSoFar; 
            // if compare_prec precSoFar precNew = -1 then failwith "addResolvingPrecedence"; 

            let itemNew = (precNew, actionNew) 
            let winner = 
                let reportConflict x1 x2 reason =
                    let reportAction (p, a) =
                        let an, astr = 
                            match a with
                            | Shift x -> "shift", sprintf "shift(%d)" x
                            | Reduce x ->
                                let nt = productionHeads.[x]
                                "reduce", productionBodies.[x]
                                |> Array.map stringOfSym
                                |> String.concat " "
                                |> sprintf "reduce(%s:%s)" (spec.NonTerminals.[nt])
                            | _ -> "", ""
                        let pstr = 
                            match p with 
                            | Some (assoc, n) -> sprintf "[explicit %s %d]" (stringOfAssoc assoc) n
                            | None  -> "noprec"
                        an, "{" + pstr + " " + astr + "}"
                    let a1n, astr1 = reportAction x1
                    let a2n, astr2 = reportAction x2
                    printfn "%s/%s error at state %d on terminal %s between %s and %s - assuming the former because %s" a1n a2n kernelIdx (fst spec.Terminals.[termIdx]) astr1 astr2 reason
                match itemSoFar,itemNew with 
                | (_,Shift s1),(_, Shift s2) -> 
                   if actionSoFar <> actionNew then 
                      reportConflict itemSoFar itemNew "internal error"
                   itemSoFar

                | (((precShift,Shift sIdx) as shiftItem), 
                   ((precReduce,Reduce prodIdx) as reduceItem))
                | (((precReduce,Reduce prodIdx) as reduceItem), 
                   ((precShift,Shift sIdx) as shiftItem)) -> 
                    match precReduce, precShift with 
                    | (Some (_,p1) as pp, Some(assocNew,p2)) -> 
                      if p1 < p2 then shiftItem
                      elif p1 > p2 then reduceItem
                      else
                        match assocNew with 
                        | LeftAssoc ->  reduceItem
                        | RightAssoc -> shiftItem
                        | NonAssoc -> 
                            if newprec then
                                pp, Error
                            else
                                reportConflict shiftItem reduceItem "we preffer shift on equal precedences"
                                incr shiftReduceConflicts;
                                shiftItem
                    | _ ->
                       reportConflict shiftItem reduceItem "we preffer shift when unable to compare precedences"
                       incr shiftReduceConflicts;
                       shiftItem
                | ((prec1,Reduce prodIdx1),(prec2, Reduce prodIdx2)) -> 
                    match prec1, prec2 with 
                    | (Some (_,p1), Some(assocNew,p2)) when newprec -> 
                        if p1 < p2 then itemNew
                        elif p1 > p2 then itemSoFar
                        else
                            "we prefer the rule earlier in the file on equal precedences"
                            |> if prodIdx1 < prodIdx2 then reportConflict itemSoFar itemNew else reportConflict itemNew itemSoFar
                            incr reduceReduceConflicts;
                            if prodIdx1 < prodIdx2 then itemSoFar else itemNew
                    | _ ->
                       "we prefer the rule earlier in the file when unable to compare precedences"
                       |> if prodIdx1 < prodIdx2 then reportConflict itemSoFar itemNew else reportConflict itemNew itemSoFar
                       incr reduceReduceConflicts;
                       if prodIdx1 < prodIdx2 then itemSoFar else itemNew
                | _ -> itemNew 
            arr.[termIdx] <- winner

          
        // This build the action table for one state. 
        let ComputeActions kernelIdx = 
            let kernel = kernels.[kernelIdx]
            let arr = Array.create spec.Terminals.Length (None, Error)

            // Compute the LR(1) items based on lookaheads
            let items =
                let accu = ResizeArray()
                for item in kernel do
                    let kernelItemIdx = { KernelIndex = kernelIdx; Item = item }
                    for  lookahead in lookaheadTable.[kernelItemIdx] do
                        accu.Add({ LR0Item = item; Lookahead = lookahead })
                ComputeClosure1 (sortedArrayofList accu)

            for item in items do
                let body = productionBodies.[item.LR0Item.ProductionIndex]
                if item.LR0Item.DotIndex < body.Length then
                    match body.[item.LR0Item.DotIndex] with
                    | TerminalIndex termIdx ->
                        let action =
                          match gotoKernel { KernelIndex = kernelIdx; SymbolIndex = TerminalIndex termIdx } with 
                          | None -> failwith "action on terminal should have found a non-empty goto state"
                          | Some gkernelItemIdx -> Shift gkernelItemIdx
                        let prec = snd spec.Terminals.[termIdx]
                        addResolvingPrecedence arr kernelIdx termIdx (prec, action)
                    | _ -> ()
                else
                    let lookahead = item.Lookahead
                    if not (isStartItem1 item) then
                        let prodIdx = item.LR0Item.ProductionIndex
                        let prec = productionPrecedences.[item.LR0Item.ProductionIndex]
                        let action = (Option.map (fun (x, y, _) -> (x, y)) prec, Reduce prodIdx)
                        addResolvingPrecedence arr kernelIdx lookahead action 
                    elif lookahead = endOfInputTerminalIdx then
                        let prec = productionPrecedences.[item.LR0Item.ProductionIndex]
                        let action = (Option.map (fun (x, y, _) -> (x, y)) prec ,Accept)
                        addResolvingPrecedence arr kernelIdx lookahead action 
                    else ()

            // If there is a single item A -> B C . and no Shift or Accept actions (i.e. only Error or Reduce, so the choice of terminal 
            // cannot affect what we do) then we emit an immediate reduce action for the rule corresponding to that item 
            // Also do the same for Accept rules. 
            let closure = (computeClosure kernel)

            // A -> B C . rules give rise to reductions in favour of errors 
            if not <| norec then
                for item in computeClosure kernel do
                    let body = productionBodies.[item.ProductionIndex]
                    if item.DotIndex = body.Length then
                        for terminalIdx = 0 to spec.Terminals.Length - 1 do
                            if snd(arr.[terminalIdx]) = Error then 
                                let prodIdx = item.ProductionIndex
                                let action = (Option.map (fun (x, y, _) -> (x, y)) productionPrecedences.[item.ProductionIndex], (if isStartItem(item) then Accept else Reduce prodIdx))
                                addResolvingPrecedence arr kernelIdx terminalIdx action

            arr

        Array.init kernels.Length ComputeActions

    // The goto table is much simpler - it is based on LR(0) kernels alone. 

    reportTime(); printf  "building goto table..."; stdout.Flush();
    let gotoTable = 
         Array.init kernels.Length (fun kernelIndex ->
            Array.init spec.NonTerminals.Length (fun nonTerminalIndex ->
                gotoKernel ({ KernelIndex = kernelIndex; SymbolIndex = NonTerminalIndex nonTerminalIndex }))         )

    reportTime(); printfn  "returning tables."; stdout.Flush();
    if !shiftReduceConflicts > 0 then printfn  "%d shift/reduce conflicts" !shiftReduceConflicts; stdout.Flush();
    if !reduceReduceConflicts > 0 then printfn  "%d reduce/reduce conflicts" !reduceReduceConflicts; stdout.Flush();
    if !shiftReduceConflicts > 0 || !reduceReduceConflicts > 0 then printfn  "consider setting precedences explicitly using %%left %%right and %%nonassoc on terminals and/or setting explicit precedence on rules using %%prec"

    /// The final results
    let states = kernels
    let prods = Array.map (fun (prod : Production) -> (prod.Head, indexOfNonTerminal.[prod.Head], prod.Body, prod.Code)) spec.Productions

    let outputPrecInfo f p = 
        match p with 
        | Some (assoc, n, symbol) -> fprintf f " (%d %s %s)" n (stringOfAssoc assoc) symbol
        | None  -> ()

    let stringOfPrecInfo p = 
        match p with 
        | Some (assoc,n) -> sprintf "(%d %s)" n (stringOfAssoc assoc)
        | None  -> ""
    
    let outputTable (f : TextWriter) (indent : int) (rows : (string * int) [] []) =
        let cols = Array.max (Array.map Array.length rows)
        let maxWidths = Array.create cols 0
        for row in rows do
            for i = 0 to row.Length - 1 do
                maxWidths.[i] <- max maxWidths.[i] (snd row.[i])
        for row in rows do
            f.Write(String(' ', indent))
            for i = 0 to row.Length - 1 do
                if i > 0 then f.Write(' ')
                f.Write(fst row.[i])
                f.Write(String(' ', maxWidths.[i] - (snd row.[i])))
            f.WriteLine()

    Option.iter (fun f -> 
        printfn  "writing tables to log"
        stdout.Flush()
        
        fprintfn f "------------------------";
        fprintfn f "states = ";
        fprintfn f "";
        for i = 0 to states.Length - 1 do
            fprintfn f "<div id=\"s%d\">state %d:</div>" i i
            
            fprintfn f "  items:"
            for item in states.[i] do
                let syms = ResizeArray(Array.map stringOfSym productionBodies.[item.ProductionIndex])
                syms.Insert(item.DotIndex, "\u25CF")
                fprintf f "    %s -&gt; %s" (spec.NonTerminals.[productionHeads.[item.ProductionIndex]]) (String.Join(' ', syms))
                fprintfn f "%a" outputPrecInfo spec.Productions.[item.ProductionIndex].PrecedenceInfo
            fprintfn f ""

            fprintfn f "  actions:"
            let rows = ResizeArray()
            for j = 0 to actionTable.[i].Length - 1 do
                let _, action = actionTable.[i].[j]
                if action <> Error then
                    let term, prec = spec.Terminals.[j]
                    let precText = stringOfPrecInfo prec
                    let termText = (if precText.Length = 0 then term else term + " " + precText) + ":"

                    let actionText =
                        match action with 
                        | Shift n -> (sprintf "shift <a href=\"#s%d\">%d</a>" n n, (6 + n.ToString().Length))
                        | Reduce prodIdx ->
                            let s = sprintf "reduce %s -&gt; %s" (spec.NonTerminals.[productionHeads.[prodIdx]]) (stringOfSyms productionBodies.[prodIdx])
                            (s, s.Length - 4)
                        | Error -> ("error", 5)
                        | Accept -> ("accept", 6)
                    
                    rows.Add([| (termText, termText.Length); actionText |])
            outputTable f 4 (rows.ToArray())
            fprintfn f ""

            fprintfn f "  gotos:"
            for j = 0 to gotoTable.[i].Length - 1 do
                match gotoTable.[i].[j] with
                | Some st -> fprintfn f "    %s: <a href=\"#s%d\">%d</a>" spec.NonTerminals.[j] st st
                | None -> ()
            fprintfn f ""
            
        fprintfn f "startStates = %s" (String.Join(";", (Array.map string startKernelIdxs)));
        fprintfn f "------------------------") logf

    let states = Array.map (fun state -> Array.map (fun (item : LR0Item) -> item.ProductionIndex) state) states

    { Productions = prods
      States = states
      StartStates = startKernelIdxs
      ActionTable = actionTable
      GotoTable = gotoTable
      EndOfInputTerminalIndex = indexOfTerminal.[endOfInputTerminal]
      ErrorTerminalIndex = errorTerminalIdx }
