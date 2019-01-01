module FsLexYacc.FsYacc.Compile

open Syntax
open Preprocess

open System
open System.IO
open System.Collections.Generic
open Printf
open Microsoft.FSharp.Collections
open System.Diagnostics

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

type KernelItemIndex =
    { KernelIndex : int
      Item : LR0Item }

type GotoItemIndex =
    { KernelIndex : int
      SymbolIndex : SymbolIndex }

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

/// Hold the results of cpmuting the LALR(1) closure of an LR(0) kernel
type Closure1Table() = 
    let t = new Dictionary<LR0Item,HashSet<TerminalIndex>>()
    member table.Add(a,b) = 
        if not (t.ContainsKey(a)) then t.[a] <- new HashSet<_>(HashIdentity.Structural)
        t.[a].Add(b)
    member table.Count  = t.Count
    member table.IEnumerable = (t :> seq<_>)
    member table.Contains(a,b) = t.ContainsKey(a) && t.[a].Contains(b)

/// A mutable table giving a lookahead set Set<Terminal> for each kernel. The terminals represent the
/// "spontaneous" items for the kernel. TODO: document this more w.r.t. the Dragon book.
type SpontaneousTable() = 
    let t = new Dictionary<KernelItemIndex,HashSet<TerminalIndex>>()
    member table.Add(a,b) = 
        if not (t.ContainsKey(a)) then t.[a] <- new HashSet<_>(HashIdentity.Structural)
        t.[a].Add(b)
    member table.Count  = t.Count
    member table.IEnumerable = (t :> seq<_>)

/// A mutable table giving a Set<KernelItemIndex> for each kernel. The kernels represent the
/// "propagate" items for the kernel. TODO: document this more w.r.t. the Dragon book.
type PropagateTable() = 
    let t = new Dictionary<KernelItemIndex,HashSet<KernelItemIndex>>()
    member table.Add(a,b) = 
        if not (t.ContainsKey(a)) then t.[a] <- new HashSet<KernelItemIndex>(HashIdentity.Structural)
        t.[a].Add(b)
    member table.Item 
      with get(a) = 
        let ok,v = t.TryGetValue(a) 
        if ok then v :> seq<_> else Seq.empty
    member table.Count  = t.Count

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
        let firstSets = Dictionary<SymbolIndex, HashSet<TerminalIndex option>>(HashIdentity.Structural)

        // For terminals, add itself (Some term) to its first-set.
        for term = 0 to spec.Terminals.Length - 1 do
            let set = HashSet(HashIdentity.Structural)
            set.Add(Some term) |> ignore
            firstSets.Add(TerminalIndex term, set)

        // For non-terminals, start with empty set.
        for nonTerm = 0 to spec.NonTerminals.Length - 1 do
            firstSets.Add(NonTerminalIndex nonTerm, HashSet(HashIdentity.Structural))
        
        let mutable added = false
        let add symbolIndex firstSetItem =
            added <- firstSets.[symbolIndex].Add(firstSetItem) || added

        let scan() =
            for prodIdx = 0 to spec.Productions.Length - 1 do
                let head = productionHeads.[prodIdx]
                let body = productionBodies.[prodIdx]
                let mutable pos = 0
                while pos < body.Length do
                    // add first symbols of production body to the first-set of this production head 
                    for firstSetItem in firstSets.[body.[pos]] do
                        if firstSetItem.IsSome then
                            add (NonTerminalIndex head) firstSetItem
                    if firstSets.[body.[pos]].Contains None then
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

        firstSets

    let firstSetOfSymbolString (str : SymbolIndex list) (term : TerminalIndex) =
        let acc = List<TerminalIndex>()
        let rec add l = 
            match l with 
            | [] -> acc.Add(term)
            | sym :: moreSyms -> 
                let firstSetOfSym = firstSetOfSymbol.[sym]
                Seq.iter (function Some v -> acc.Add(v) | None -> ()) firstSetOfSym
                if firstSetOfSym.Contains(None) then
                    add moreSyms 
        add str
        Set.ofSeq acc
    
    let firstSetOfSymbolString = memoize2 firstSetOfSymbolString
    
    let createItem productionIndex = { ProductionIndex = productionIndex; DotIndex = 0 }
    let precedenceOfItem (item : LR0Item) = productionPrecedences.[item.ProductionIndex]
    let headOfItem (item : LR0Item) = productionHeads.[item.ProductionIndex]

    let rsym_of_item (item : LR0Item) = 
        let body = productionBodies.[item.ProductionIndex]
        if item.DotIndex < body.Length then
            Some body.[item.DotIndex]
        elif item.DotIndex = body.Length then
            None
        else failwith "unreachable"

    let advanceOfItem (item : LR0Item) = { item with DotIndex = item.DotIndex + 1 }
    //let startNonTerminalsSet = Set.ofArray (Array.map (fun s -> indexOfNonTerminal.[s]) spec.StartSymbols)
    let isStartItem (item : LR0Item) = Array.contains (spec.NonTerminals.[productionHeads.[item.ProductionIndex]]) spec.StartSymbols
        //startNonTerminalsSet.Contains(headOfItem item)
    //let isKernelItem (item : Item) =
    //    let ans = isStartItem item || item.DotIndex <> 0
    //    if not ans then failwith "unreachable"
    //    ans

    let computeClosure (itemSet : Set<LR0Item>) =
        let mutable accu = itemSet
        let queue = Queue<LR0Item>(itemSet)
        while queue.Count > 0 do
            let item = queue.Dequeue()
            let body = productionBodies.[item.ProductionIndex]
            if item.DotIndex < body.Length then
                match body.[item.DotIndex] with
                | NonTerminalIndex ni ->
                    for prod in productionsOfNonTerminal.[ni] do
                        let newItem = createItem prod
                        if not (Set.contains newItem accu) then
                            accu <- Set.add newItem accu
                            queue.Enqueue newItem
                | _ -> ()
        accu

    let computeClosure = memoize1 computeClosure

    // Goto set of a kernel of LR(0) nonTerminals, items etc 
    // Input is kernel, output is kernel
    let computeGotoOfKernel kernel sym = 
        let accu = List()
        for item in computeClosure kernel do
            let body = productionBodies.[item.ProductionIndex]
            if item.DotIndex < body.Length && body.[item.DotIndex] = sym then
                accu.Add(advanceOfItem item) 
        Set.ofSeq accu

    let computeGotosOfKernel kernel = 
        let accu = Dictionary<SymbolIndex, HashSet<LR0Item>>(HashIdentity.Structural)
        for item in computeClosure kernel do
            let body = productionBodies.[item.ProductionIndex]
            if item.DotIndex < body.Length then
                let sym = body.[item.DotIndex]
                if not (accu.ContainsKey(sym)) then
                    accu.[sym] <- HashSet(HashIdentity.Structural)
                accu.[sym].Add(advanceOfItem item) |> ignore
        Array.map Set.ofSeq (Array.ofSeq accu.Values)

    // Build the full set of LR(0) kernels 
    reportTime(); printf "building kernels..."; stdout.Flush();
    let startItems = Array.map (fun startSymbol -> createItem productionsOfNonTerminal.[indexOfNonTerminal.[startSymbol]].[0]) spec.StartSymbols
    let startKernels = Array.map Set.singleton startItems
    let kernels = 
        let mutable accu = Set.empty
        let queue = Queue(startKernels)
        while queue.Count > 0 do
            let kernel = queue.Dequeue()
            if not (accu.Contains(kernel)) then
                accu <- accu.Add(kernel)
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
        if gset.IsEmpty then None else Some (indexOfKernel.[gset])

    let gotoKernel = memoize1 gotoKernel

    // This is used to compute the closure of an LALR(1) kernel 
    //
    // For each item [A --> X.BY, a] in I
    //   For each production B -> g in G'
    //     For each terminal b in FIRST(Ya)
    //        such that [B --> .g, b] is not in I do
    //            add [B --> .g, b] to I
    
    let ComputeClosure1 (iset : (LR0Item * Set<TerminalIndex>) list) = 
        let acc = new Closure1Table()
        let queue = Queue(iset)
        while queue.Count > 0 do
            let item, pretokens = queue.Dequeue()
            for pretoken in pretokens do
                if not (acc.Contains(item, pretoken)) then
                    acc.Add(item, pretoken) |> ignore
                    let body = productionBodies.[item.ProductionIndex]
                    if item.DotIndex < body.Length then
                        match body.[item.DotIndex] with
                        | NonTerminalIndex ntB -> 
                             let firstSet = firstSetOfSymbolString (Array.toList body.[(item.DotIndex + 1)..]) pretoken
                             for prodIdx in productionsOfNonTerminal.[ntB] do
                                 queue.Enqueue (createItem prodIdx, firstSet)
                        | TerminalIndex _ -> ()
        acc

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

        
    let spontaneous, propagate  =
        let closure1OfItemWithDummy item = ComputeClosure1 [(item, Set.singleton dummyLookaheadIdx)] 
        let closure1OfItemWithDummy = memoize1 closure1OfItemWithDummy

        let spontaneous = new SpontaneousTable()
        let propagate = new PropagateTable()

        for kernelIdx = 0 to kernels.Length - 1 do
            printf  "."; stdout.Flush();
            //printf  "kernelIdx = %d\n" kernelIdx; stdout.Flush();
            let kernel = kernels.[kernelIdx]
            for item in kernel do  
                let itemIdx = { KernelIndex = kernelIdx; Item = item }
                let jset = closure1OfItemWithDummy item
                //printf  "#jset = %d\n" jset.Count; stdout.Flush();
                for (KeyValue(closureItem, lookaheadTokens)) in jset.IEnumerable do
                    match rsym_of_item closureItem with 
                    | None -> ()
                    | Some rsym ->
                         match gotoKernel { KernelIndex = kernelIdx; SymbolIndex = rsym } with 
                         | None -> ()
                         | Some gotoKernelIdx ->
                              let gotoItem = advanceOfItem closureItem
                              let gotoItemIdx = { KernelIndex = gotoKernelIdx; Item = gotoItem }
                              for lookaheadToken in lookaheadTokens do
                                  if lookaheadToken = dummyLookaheadIdx 
                                  then propagate.Add(itemIdx, gotoItemIdx) |> ignore
                                  else spontaneous.Add(gotoItemIdx, lookaheadToken) |> ignore


        spontaneous, propagate
   
    // Repeatedly use the "spontaneous" and "propagate" maps to build the full set 
    // of lookaheads for each LR(0) kernelItem.   
    reportTime(); printf  "building lookahead table..."; stdout.Flush();
    let lookaheadTable = 
        let queue = Queue()

        // Seed the table with the startKernelItems and the spontaneous info
        for idx in startKernelItemIdxs do
            queue.Enqueue (idx,endOfInputTerminalIdx)
        for KeyValue(kernelItemIdx,lookaheads) in spontaneous.IEnumerable do
            for lookahead in lookaheads do
                queue.Enqueue (kernelItemIdx,lookahead) 

        let acc = Dictionary<KernelItemIndex, HashSet<TerminalIndex>>(HashIdentity.Structural)

        let contains kernelItemIndex lookahead =
            match acc.TryGetValue(kernelItemIndex) with
            | true, s -> s.Contains(lookahead)
            | false, _ -> false

        let add kernelItemIndex lookahead =
            let s =
                match acc.TryGetValue(kernelItemIndex) with
                | true, s -> s
                | false, _ ->
                    let s = HashSet(HashIdentity.Structural)
                    acc.Add(kernelItemIndex, s)
                    s
            s.Add(lookahead) |> ignore

        // Compute the closure
        while queue.Count > 0 do
            let kernelItemIdx, lookahead = queue.Dequeue()
            add kernelItemIdx lookahead
            for gotoKernelIdx in propagate.[kernelItemIdx] do
                if not (contains gotoKernelIdx lookahead) then 
                    queue.Enqueue(gotoKernelIdx, lookahead)
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

            //printf  "building lookahead table LR(1) items for kernelIdx %d\n" kernelIdx; stdout.Flush();

            // Compute the LR(1) items based on lookaheads
            let items = 
                 [ for item in kernel do
                     let kernelItemIdx = { KernelIndex = kernelIdx; Item = item }
                     let lookaheads = Set.ofSeq (lookaheadTable.[kernelItemIdx])
                     yield (item, lookaheads) ]
                 |> ComputeClosure1

            for (KeyValue(item, lookaheads)) in items.IEnumerable do

                let nonTermA = headOfItem item
                match rsym_of_item item with 
                | Some (TerminalIndex termIdx) -> 
                    let action =
                      match gotoKernel { KernelIndex = kernelIdx; SymbolIndex = TerminalIndex termIdx } with 
                      | None -> failwith "action on terminal should have found a non-empty goto state"
                      | Some gkernelItemIdx -> Shift gkernelItemIdx
                    let prec = snd spec.Terminals.[termIdx]
                    addResolvingPrecedence arr kernelIdx termIdx (prec, action) 
                | None ->
                    for lookahead in lookaheads do
                        if not (isStartItem(item)) then
                            let prodIdx = item.ProductionIndex
                            let prec = precedenceOfItem item
                            let action = (prec, Reduce prodIdx)
                            addResolvingPrecedence arr kernelIdx lookahead action 
                        elif lookahead = endOfInputTerminalIdx then
                            let prec = precedenceOfItem item
                            let action = (prec,Accept)
                            addResolvingPrecedence arr kernelIdx lookahead action 
                        else ()
                | _ -> ()

            // If there is a single item A -> B C . and no Shift or Accept actions (i.e. only Error or Reduce, so the choice of terminal 
            // cannot affect what we do) then we emit an immediate reduce action for the rule corresponding to that item 
            // Also do the same for Accept rules. 
            let closure = (computeClosure kernel)

            // A -> B C . rules give rise to reductions in favour of errors 
            if not <| norec then
                for item in computeClosure kernel do
                    let prec = precedenceOfItem item
                    match rsym_of_item item with 
                    | None ->
                        for terminalIdx = 0 to spec.Terminals.Length - 1 do
                            if snd(arr.[terminalIdx]) = Error then 
                                let prodIdx = item.ProductionIndex
                                let action = (prec, (if isStartItem(item) then Accept else Reduce prodIdx))
                                addResolvingPrecedence arr kernelIdx terminalIdx action
                    | _  -> ()

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
        | Some (assoc,n) -> fprintf f "explicit %s %d" (stringOfAssoc assoc) n
        | None  -> fprintf f "noprec"

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
                fprintf f "    %s -&gt; %s" (spec.NonTerminals.[(headOfItem item)]) (String.Join(' ', syms))
                fprintfn f " (%a)" outputPrecInfo spec.Productions.[item.ProductionIndex].PrecedenceInfo
            fprintfn f ""

            fprintfn f "  actions:"
            for j = 0 to actionTable.[i].Length - 1 do
                let prec, action = actionTable.[i].[j]
                let term, _ = spec.Terminals.[j]
                fprintf f "    %s (%a): " term outputPrecInfo prec
                match action with 
                | Shift n -> fprintfn f "shift <a href=\"#s%d\">%d</a>" n n 
                | Reduce prodIdx ->  fprintfn f "reduce %s -&gt; %s" (spec.NonTerminals.[productionHeads.[prodIdx]]) (stringOfSyms productionBodies.[prodIdx])
                | Error ->  fprintfn f "error"
                | Accept -> fprintfn f "accept"
            fprintfn f ""

            fprintfn f "  gotos:"
            for j = 0 to gotoTable.[i].Length - 1 do
                match gotoTable.[i].[j] with
                | Some st -> fprintfn f "    %s: <a href=\"#s%d\">%d</a>" spec.NonTerminals.[j] st st
                | None -> ()
            fprintfn f ""
            
        fprintfn f "startStates = %s" (String.Join(";", (Array.map string startKernelIdxs)));
        fprintfn f "------------------------") logf

    let states = Array.map (fun state -> Array.map (fun (item : LR0Item) -> item.ProductionIndex) (Set.toArray state)) states

    { Productions = prods
      States = states
      StartStates = startKernelIdxs
      ActionTable = actionTable
      GotoTable = gotoTable
      EndOfInputTerminalIndex = indexOfTerminal.[endOfInputTerminal]
      ErrorTerminalIndex = errorTerminalIdx }
