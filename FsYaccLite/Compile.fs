﻿module FsLexYacc.FsYacc.Compile

open System
open System.Collections.Generic
open System.IO
open System.Diagnostics
open System.Drawing

open Printf

open Syntax
open Preprocess

type TerminalIndex = int
type NonTerminalIndex = int
type SymbolIndex = int
type ProductionIndex = int
type TerminalIndexOrEpsilon = int
type KernelIndex = int

type LR0Item =
    { ProductionIndex : ProductionIndex
      DotIndex : int }

let createLR0Item productionIndex = { ProductionIndex = productionIndex; DotIndex = 0 }

type KernelItemIndex =
    { KernelIndex : KernelIndex
      Item : LR0Item }

type LR1Item =
    { LR0Item : LR0Item
      Lookahead : TerminalIndex }

type Action = 
  | Shift of stateIndex : int
  | Reduce of productionIndex : int
  | Accept
  | Error
    
type CompiledProduction =
    {
        HeadNonTerminalIndex : NonTerminalIndex
        BodySymbolIndexes : SymbolIndex []
    }

type Compiled =
    {
        FirstSets : Dictionary<SymbolIndex, HashSet<TerminalIndexOrEpsilon>>
        Productions : CompiledProduction []
        States : ProductionIndex [] []
        Kernels : LR0Item [] []
        StartStates : int []
        ActionTable : Action [] []
        GotoTable : int option [] [] 
        EndOfInputTerminalIndex : int
        ErrorTerminalIndex : int
    }

let sortedArrayofList (l : List<'T>) =
    let ary = l.ToArray()
    Array.sortInPlace ary
    ary

let sortedArrayOfHashSet (hs : HashSet<'T>) =
    let ary = Array.zeroCreate hs.Count
    hs.CopyTo(ary)
    Array.sortInPlace ary
    ary

let sortedArrayOfSeq seq =
    let ary = Array.ofSeq seq
    Array.sortInPlace ary
    ary

let memoize1 f =
    let dict = Dictionary(HashIdentity.Structural)
    fun a ->
        match dict.TryGetValue(a) with
        | true, x -> x
        | false, _ ->
            let b = f a
            dict.[a] <- b
            b

let memoize2 f =
    let dict = Dictionary(HashIdentity.Structural)
    fun a b ->
        match dict.TryGetValue((a, b)) with
        | true, c -> c
        | false, _ ->
            let c = f a b
            dict.[(a, b)] <- c
            c

let memoize3 f =
    let dict = Dictionary(HashIdentity.Structural)
    fun a b c ->
        match dict.TryGetValue((a, b, c)) with
        | true, d -> d
        | false, _ ->
            let d = f a b c
            dict.[(a, b, c)] <- d
            d

type MultiDictionary<'T, 'U when 'T : equality and 'U : equality> = Dictionary<'T, HashSet<'U>>
let MultiDictionary_Create<'T, 'U when 'T : equality and 'U : equality>() : MultiDictionary<'T, 'U> = Dictionary<'T, HashSet<'U>>(HashIdentity.Structural)

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

let [<Literal>] Epsilon = -1

let compile (newprec:bool) (norec:bool) (spec : Preprocessed) =
    let stopWatch = Stopwatch.StartNew()
    let reportTime() =
        printfn "time: %d(ms)" stopWatch.ElapsedMilliseconds
        stopWatch.Restart()

    let indexOfNonTerminal =
        let d = Dictionary<string, NonTerminalIndex>(HashIdentity.Structural)
        for i = 0 to spec.NonTerminals.Length - 1 do
            d.Add(spec.NonTerminals.[i], i)
        d

    let indexOfTerminal =
        let d = Dictionary<string, int>(HashIdentity.Structural)
        for i = 0 to spec.Terminals.Length - 1 do
            d.Add(fst spec.Terminals.[i], i)
        d
    
    let symbolIndexIsTerminal (i : SymbolIndex) = i < spec.Terminals.Length
    let symbolIndexIsNonTerminal (i : SymbolIndex) = spec.Terminals.Length <= i
    let symbolIndexOfTerminalIndex (i : TerminalIndex) : SymbolIndex = i
    let symbolIndexOfNonTerminalIndex (i : NonTerminalIndex) : SymbolIndex = spec.Terminals.Length + i 
    let terminalIndexOfSymbolIndex (i : SymbolIndex) : TerminalIndex = i
    let nonTerminalIndexOfSymbolIndex (i : SymbolIndex) : NonTerminalIndex = i - spec.Terminals.Length

    let indexOfSymbol (symbol : string) =
        match indexOfTerminal.TryGetValue symbol with
        | true, i -> symbolIndexOfTerminalIndex i
        | false, _ ->
            match indexOfNonTerminal.TryGetValue symbol with
            | true, i -> symbolIndexOfNonTerminalIndex i
            | false, _ -> failwith "unreachable"

    let productions =
        Array.map
            (fun (prod : Production) ->
                { HeadNonTerminalIndex = indexOfNonTerminal.[prod.Head]
                  BodySymbolIndexes = Array.map indexOfSymbol prod.Body }) spec.Productions

    let productionsOfNonTerminal : ProductionIndex [] [] =
        let table = Array.init spec.NonTerminals.Length (fun _ -> ResizeArray<int>())
        for i = 0 to spec.Productions.Length - 1 do
            table.[productions.[i].HeadNonTerminalIndex].Add(i)
        Array.map (fun (l : ResizeArray<int>) -> l.ToArray()) table

    let dummyLookaheadIdx = indexOfTerminal.[dummyLookahead]
    let endOfInputTerminalIndex = indexOfTerminal.[endOfInputTerminal]

    printf  "computing first function..."; stdout.Flush();

    let firstSetOfSymbol =
        let accu = Dictionary<SymbolIndex, HashSet<TerminalIndexOrEpsilon>>(HashIdentity.Structural)

        // For terminals, add itself to its FIRST set.
        for terminalIndex = 0 to spec.Terminals.Length - 1 do
            let set = HashSet(HashIdentity.Structural)
            set.Add(terminalIndex) |> ignore
            accu.Add(symbolIndexOfTerminalIndex terminalIndex, set)

        // For non-terminals, start with empty set.
        for nonTerminalIndex = 0 to spec.NonTerminals.Length - 1 do
            accu.Add(symbolIndexOfNonTerminalIndex nonTerminalIndex, HashSet(HashIdentity.Structural))
        
        let mutable added = false
        let add symbolIndex firstSetItem =
            added <- accu.[symbolIndex].Add(firstSetItem) || added

        let scan() =
            for prodIdx = 0 to spec.Productions.Length - 1 do
                let head = productions.[prodIdx].HeadNonTerminalIndex
                let body = productions.[prodIdx].BodySymbolIndexes
                let mutable pos = 0
                while pos < body.Length do
                    // add first symbols of production body to the first-set of this production head 
                    for firstSetItem in accu.[body.[pos]] do
                        if firstSetItem <> Epsilon then
                            add (symbolIndexOfNonTerminalIndex head) firstSetItem
                    if accu.[body.[pos]].Contains Epsilon then
                        // the symbol at pos can be empty, therefore go through the following symbols
                        pos <- pos + 1
                    else
                        // otherwise, stop here
                        pos <- Int32.MaxValue
                if pos = body.Length then
                    // the scan for production body symbols has been gone through the end of the body
                    // therefore all symbols in production body contains epsilon
                    // therefore the FIRST set for this non-terminal should contain epsilon
                    add (symbolIndexOfNonTerminalIndex head) Epsilon

        // repeat scan until it becomes making no difference
        scan()
        while added do
            added <- false
            scan()

        accu

    let firstSetOfPartOfProductionBodyWithLookahead (productionIndex : int) (startPos : int) (lookahead : TerminalIndex) =
        let accu = HashSet<TerminalIndex>(HashIdentity.Structural)
        let body = productions.[productionIndex].BodySymbolIndexes
        let mutable pos = startPos

        while pos < body.Length do
            let firstSet = firstSetOfSymbol.[body.[pos]]
            for first in firstSet do
                match first with
                | Epsilon -> ()
                | terminalIndex -> accu.Add(terminalIndex) |> ignore
            if firstSet.Contains(Epsilon) then
                pos <- pos + 1
            else
                pos <- Int32.MaxValue
        if pos = body.Length then
            accu.Add(lookahead) |> ignore

        accu
    
    let firstSetOfPartOfProductionBodyWithLookahead = memoize3 firstSetOfPartOfProductionBodyWithLookahead

    let advanceOfItem (item : LR0Item) = { item with DotIndex = item.DotIndex + 1 }

    let computeClosure (itemSet : LR0Item []) =
        let accu = HashSet(HashIdentity.Structural)
        for item in itemSet do
            accu.Add(item) |> ignore

        let queue = Queue<LR0Item>(itemSet)
        while queue.Count > 0 do
            let item = queue.Dequeue()
            let body = productions.[item.ProductionIndex].BodySymbolIndexes
            if item.DotIndex < body.Length then
                let symbol = body.[item.DotIndex]
                if symbolIndexIsNonTerminal symbol then
                    for prod in productionsOfNonTerminal.[nonTerminalIndexOfSymbolIndex symbol] do
                        let newItem = createLR0Item prod
                        if accu.Add(newItem) then
                            queue.Enqueue newItem

        sortedArrayOfHashSet accu

    let computeClosure = memoize1 computeClosure

    let computeGotoOfKernel (kernel : LR0Item []) (symbol : SymbolIndex) : LR0Item [] = 
        let accu = List()

        for item in computeClosure kernel do
            let body = productions.[item.ProductionIndex].BodySymbolIndexes
            if item.DotIndex < body.Length && body.[item.DotIndex] = symbol then
                accu.Add(advanceOfItem item)

        sortedArrayofList accu

    let computeGotosOfKernel (kernel : LR0Item []) : LR0Item [] [] = 
        let accu = MultiDictionary_Create<SymbolIndex, LR0Item>()

        for item in computeClosure kernel do
            let body = productions.[item.ProductionIndex].BodySymbolIndexes
            if item.DotIndex < body.Length then
                MultiDictionary_Add accu body.[item.DotIndex] (advanceOfItem item)
        
        Array.map (fun symbolIndex -> sortedArrayOfHashSet accu.[symbolIndex]) (sortedArrayOfSeq accu.Keys)

    reportTime(); printf "building kernels..."; stdout.Flush();
    let startItems = Array.map (fun startSymbol -> createLR0Item productionsOfNonTerminal.[indexOfNonTerminal.[startSymbol]].[0]) spec.StartSymbols
    let startKernels = Array.map (fun x -> [| x |]) startItems
    let kernels =
        let queue = Queue(startKernels)
        let accuSet = HashSet(HashIdentity.Structural)
        let accuList = List()
        while queue.Count > 0 do
            let kernel = queue.Dequeue()
            if accuSet.Add(kernel) then
                accuList.Add(kernel)
                for goto in computeGotosOfKernel kernel do
                    queue.Enqueue(goto)

        accuList.ToArray()
    
    reportTime(); printf "building kernel table..."; stdout.Flush();

    let indexOfKernel =
        let d = Dictionary(HashIdentity.Structural)
        for i = 0 to kernels.Length - 1 do
            d.Add(kernels.[i], i)
        d
    
    let startKernelIdxs = Array.map (fun kernel -> indexOfKernel.[kernel]) startKernels
    let startKernelItemIndexs = Array.map2 (fun kernel item -> { KernelIndex = kernel; Item = item }) startKernelIdxs startItems

    reportTime(); printf "computing lookahead relations..."; stdout.Flush();

    let gotoKernel (kernelIndex : KernelIndex) (symbolIndex : SymbolIndex) : KernelIndex option = 
        let gset = computeGotoOfKernel (kernels.[kernelIndex]) symbolIndex
        if gset.Length = 0 then None else Some (indexOfKernel.[gset])

    let gotoKernel = memoize2 gotoKernel
    
    let computeLR1Closure (itemSet : LR1Item []) = 
        let accu = HashSet<LR1Item>(HashIdentity.Structural)
        let queue = Queue(itemSet)
        while queue.Count > 0 do
            let item = queue.Dequeue()
            if accu.Add(item) then
                let body = productions.[item.LR0Item.ProductionIndex].BodySymbolIndexes
                if item.LR0Item.DotIndex < body.Length then
                    let symbol = body.[item.LR0Item.DotIndex]
                    if symbolIndexIsNonTerminal symbol then
                        let firstSet = firstSetOfPartOfProductionBodyWithLookahead item.LR0Item.ProductionIndex (item.LR0Item.DotIndex + 1) item.Lookahead
                        for productionIndex in productionsOfNonTerminal.[nonTerminalIndexOfSymbolIndex symbol] do
                            for lookahead in firstSet do
                                queue.Enqueue({ LR0Item = createLR0Item productionIndex; Lookahead = lookahead})

        sortedArrayOfHashSet accu

    let computeLR1ClosureOfLR0ItemWithDummy (item : LR0Item) = computeLR1Closure [| { LR0Item = item; Lookahead = dummyLookaheadIdx } |]
    let computeLR1ClosureOfLR0ItemWithDummy = memoize1 computeLR1ClosureOfLR0ItemWithDummy
        
    let spontaneous, propagate =

        let spontaneous = HashSet<KernelItemIndex * TerminalIndex>(HashIdentity.Structural)
        let propagate = MultiDictionary_Create<KernelItemIndex, KernelItemIndex>()

        for kernelIndex = 0 to kernels.Length - 1 do
            printf  "."; stdout.Flush();
            for kernelItem in kernels.[kernelIndex] do  
                for lr1Item in computeLR1ClosureOfLR0ItemWithDummy kernelItem do
                    let body = productions.[lr1Item.LR0Item.ProductionIndex].BodySymbolIndexes
                    if lr1Item.LR0Item.DotIndex < body.Length then
                        match gotoKernel kernelIndex body.[lr1Item.LR0Item.DotIndex] with 
                        | None -> ()
                        | Some gotoKernelIndex ->
                            let gotoItemIndex = { KernelIndex = gotoKernelIndex; Item = advanceOfItem lr1Item.LR0Item }
                            let lookaheadToken = lr1Item.Lookahead
                            if lookaheadToken = dummyLookaheadIdx then
                                MultiDictionary_Add propagate { KernelIndex = kernelIndex; Item = kernelItem } gotoItemIndex |> ignore
                            else
                                spontaneous.Add(gotoItemIndex, lookaheadToken) |> ignore

        spontaneous, propagate
   
    reportTime(); printf  "building lookahead table..."; stdout.Flush();
    let lookaheadTable = 
        let queue = Queue<KernelItemIndex * TerminalIndex>()

        for kernelItemIndex in startKernelItemIndexs do
            queue.Enqueue(kernelItemIndex, endOfInputTerminalIndex)
        for s in spontaneous do
            queue.Enqueue(s)

        let accu = MultiDictionary_Create<KernelItemIndex, TerminalIndex>()

        while queue.Count > 0 do
            let kernelItemIndex, lookahead = queue.Dequeue()
            MultiDictionary_Add accu kernelItemIndex lookahead
            match propagate.TryGetValue(kernelItemIndex) with
            | true, kernelItemsPropagateTo ->
                for kenerlItem in kernelItemsPropagateTo do
                    if not (MultiDictionary_Contains accu kenerlItem lookahead) then 
                        queue.Enqueue(kenerlItem, lookahead)
            |  false, _ -> ()
        
        accu

    let stringOfSym (symbolIndex : SymbolIndex) = spec.Symbols.[symbolIndex]
    let isStartItem (item : LR0Item) = Array.contains (spec.NonTerminals.[productions.[item.ProductionIndex].HeadNonTerminalIndex]) spec.StartSymbols
    let isStartItem1 (item : LR1Item) = isStartItem item.LR0Item    

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
                                let nt = productions.[x].HeadNonTerminalIndex
                                "reduce", productions.[x].BodySymbolIndexes
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
                    for lookahead in lookaheadTable.[kernelItemIdx] do
                        accu.Add({ LR0Item = item; Lookahead = lookahead })
                computeLR1Closure (sortedArrayofList accu)

            for item in items do
                let body = productions.[item.LR0Item.ProductionIndex].BodySymbolIndexes
                if item.LR0Item.DotIndex < body.Length then
                    let symbol = body.[item.LR0Item.DotIndex]
                    if symbolIndexIsTerminal symbol then
                        let terminalIndex = terminalIndexOfSymbolIndex symbol
                        let action =
                          match gotoKernel kernelIdx symbol with 
                          | None -> failwith "action on terminal should have found a non-empty goto state"
                          | Some gkernelItemIdx -> Shift gkernelItemIdx
                        let prec = snd spec.Terminals.[terminalIndex]
                        addResolvingPrecedence arr kernelIdx terminalIndex (prec, action)
                else
                    let lookahead = item.Lookahead
                    if not (isStartItem1 item) then
                        let prodIdx = item.LR0Item.ProductionIndex
                        let prec = spec.Productions.[item.LR0Item.ProductionIndex].PrecedenceInfo
                        let action = (Option.map (fun (x, y, _) -> (x, y)) prec, Reduce prodIdx)
                        addResolvingPrecedence arr kernelIdx lookahead action 
                    elif lookahead = endOfInputTerminalIndex then
                        let prec = spec.Productions.[item.LR0Item.ProductionIndex].PrecedenceInfo
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
                    let body = productions.[item.ProductionIndex].BodySymbolIndexes
                    if item.DotIndex = body.Length then
                        for terminalIdx = 0 to spec.Terminals.Length - 1 do
                            if snd(arr.[terminalIdx]) = Error then 
                                let prodIdx = item.ProductionIndex
                                let action = (Option.map (fun (x, y, _) -> (x, y)) spec.Productions.[item.ProductionIndex].PrecedenceInfo, (if isStartItem(item) then Accept else Reduce prodIdx))
                                addResolvingPrecedence arr kernelIdx terminalIdx action

            Array.map snd arr

        Array.init kernels.Length ComputeActions

    // The goto table is much simpler - it is based on LR(0) kernels alone. 

    reportTime(); printf  "building goto table..."; stdout.Flush();
    let gotoTable = 
         Array.init kernels.Length (fun kernelIndex ->
            Array.init spec.NonTerminals.Length (fun nonTerminalIndex ->
                let symbolIndex = symbolIndexOfNonTerminalIndex nonTerminalIndex
                gotoKernel  kernelIndex (symbolIndex)))

    reportTime(); printfn  "returning tables."; stdout.Flush();
    if !shiftReduceConflicts > 0 then printfn  "%d shift/reduce conflicts" !shiftReduceConflicts; stdout.Flush();
    if !reduceReduceConflicts > 0 then printfn  "%d reduce/reduce conflicts" !reduceReduceConflicts; stdout.Flush();
    if !shiftReduceConflicts > 0 || !reduceReduceConflicts > 0 then printfn  "consider setting precedences explicitly using %%left %%right and %%nonassoc on terminals and/or setting explicit precedence on rules using %%prec"

    { FirstSets = firstSetOfSymbol
      Productions = productions
      States = Array.map (fun state -> Array.map (fun (item : LR0Item) -> item.ProductionIndex) state) kernels
      Kernels = kernels
      StartStates = startKernelIdxs
      ActionTable = actionTable
      GotoTable = gotoTable
      EndOfInputTerminalIndex = indexOfTerminal.[endOfInputTerminal]
      ErrorTerminalIndex = indexOfTerminal.[errorTerminal] }
