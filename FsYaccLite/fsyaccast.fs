// (c) Microsoft Corporation 2005-2007.
module FsLexYacc.FsYacc.AST

open System
open System.Collections.Generic
open Printf
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Text.Lexing
open System.Net.Http.Headers

type Identifier = string
type Code = string * Position
type Rule = Rule of Identifier list * Identifier option * Code option
type Associativity = LeftAssoc | RightAssoc | NonAssoc

type ParserSpec= 
    { Header : Code
      Tokens : (Identifier * string option) list
      Types : (Identifier * string) list
      Associativities : (Identifier * Associativity) list list
      StartSymbols : Identifier list
      Rules : (Identifier * Rule list) list }
      
//---------------------------------------------------------------------
// PreProcess Raw Parser Spec AST

type PrecedenceInfo = (Associativity * int) option
type Symbol = Terminal of string | NonTerminal of string

type Production =
    { Head : string
      Body : Symbol array
      PrecedenceInfo : PrecedenceInfo
      Code : Code option }

type ProcessedParserSpec = 
    { Terminals: (string * PrecedenceInfo) array
      NonTerminals: string array
      Productions: Production array
      StartSymbols: string array }

let endOfInputTerminal = "$"
let dummyLookahead = "#"
let errorTerminal = "error"

let processParserSpecAst (spec : ParserSpec) =
    
    let explicitPrecInfo = 
        let levels = spec.Associativities.Length
        spec.Associativities 
        |> List.mapi (fun i precSpecs -> List.map (fun (precSym, assoc) -> (precSym, (assoc, levels - i - 1))) precSpecs)
        |> List.concat
    
    for key, _ in explicitPrecInfo |> Seq.countBy fst |> Seq.filter (fun (_, n) -> n > 1)  do
        failwithf "%s is given two associativities" key
    
    let explicitPrecInfo = Map.ofList explicitPrecInfo
    let terminals = Array.append (Array.map fst (Array.ofList spec.Tokens)) [| errorTerminal |]
    let terminalSet = Set.ofArray terminals
       
    let productions =  
        spec.Rules |> List.mapi (fun i (nonterm,rules) -> 
            rules |> List.mapi (fun j (Rule(syms,precsym,code)) -> 
                let precInfo = 
                    let precsym = List.foldBack (fun x acc -> match acc with Some _ -> acc | None -> match x with z when terminalSet.Contains z -> Some z | _ -> acc) syms precsym
                    match precsym with 
                    | Some sym -> if explicitPrecInfo.ContainsKey(sym) then Some explicitPrecInfo.[sym] else None
                    | None -> None
                { Head = nonterm; PrecedenceInfo = precInfo; Body = Array.map (fun s -> if terminalSet.Contains s then Terminal s else NonTerminal s) (Array.ofList syms); Code = code }))
         |> List.concat
         |> Array.ofList

    let nonTerminals = Array.map fst (Array.ofList spec.Rules)
    let nonTerminalSet = Set.ofArray nonTerminals

    let checkNonTerminal nt =  
        if nt <> errorTerminal && not (nonTerminalSet.Contains(nt)) then 
            failwith (sprintf "NonTerminal '%s' has no productions" nt)

    for prod in productions do
        for sym in prod.Body do 
           match sym with 
           | NonTerminal nt -> checkNonTerminal nt 
           | Terminal t -> if not (terminalSet.Contains t) then failwith (sprintf "token %s is not declared" t)
           
    if spec.StartSymbols = [] then (failwith "at least one %start declaration is required\n")

    for nt,_ in spec.Types do 
        checkNonTerminal nt

    let terminals = Array.map (fun t -> (t, explicitPrecInfo.TryFind t)) terminals
    let startSymbols = Array.ofList spec.StartSymbols
    
    // Augment the grammar 
    let fakeStartSymbols = Array.map (fun nt -> "_start" + nt) startSymbols
    let nonTerminals = Array.append fakeStartSymbols nonTerminals
    let terminals = Array.append terminals [| (dummyLookahead, None); (endOfInputTerminal, None) |]
    let productions =
        Array.append
            (Array.map2
                (fun fakeStartNonTerminal startSymbol ->
                    { Head = fakeStartNonTerminal; PrecedenceInfo = None; Body = [| NonTerminal startSymbol |]; Code = None })
                fakeStartSymbols
                startSymbols)
            productions

    { Terminals = terminals
      NonTerminals = nonTerminals
      Productions = productions
      StartSymbols = fakeStartSymbols }

//-------------------------------------------------
// Process LALR(1) grammars to tables

/// Part of the output of CompilerLalrParserSpec
type Action = 
  | Shift of stateIndex : int
  | Reduce of productionIndex : int
  | Accept
  | Error

//---------------------------------------------------------------------
// Output Raw Parser Spec AST

let stringOfAssoc (assoc : Associativity) =
    match assoc with
    | LeftAssoc -> "left"
    | RightAssoc -> "right"
    | NonAssoc -> "nonassoc"
    
let outputPrecInfo os p = 
    match p with 
    | Some (assoc,n) -> fprintf os "explicit %s %d" (stringOfAssoc assoc) n
    | None  -> fprintf os "noprec"

type TerminalIndex = int
type NonTerminalIndex = int
type ProductionIndex = int

type SymbolIndex =
    | TerminalIndex of int : TerminalIndex
    | NonTerminalIndex of int : NonTerminalIndex

type Item =
    { ProductionIndex : ProductionIndex
      DotIndex : int }

type KernelItemIndex =
    { KernelIndex : int
      Item : Item }

type GotoItemIndex =
    { KernelIndex : int
      SymbolIndex : SymbolIndex }

/// Create a work list and loop until it is exhausted, calling a worker function for
/// each element. Pass a function to queue additional work on the work list 
/// to the worker function
let processWorkList (start : 'a list) (f : ('a -> unit) -> 'a -> unit) =
    let work = ref start
    let queueWork x =
        work := x :: !work
    let rec loop() =
        match !work with 
        | [] -> ()
        | x :: t -> 
            work := t
            f queueWork x
            loop()
    loop()

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

/// A mutable table maping kernels to sets of lookahead tokens
type LookaheadTable() = 
    let t = new Dictionary<KernelItemIndex, Set<TerminalIndex>>()
    member table.Add(x,y) = 
        let prev = if t.ContainsKey(x) then t.[x] else Set.empty 
        t.[x] <- prev.Add(y)
    member table.Contains(x,y) = t.ContainsKey(x) && t.[x].Contains(y)
    member table.GetLookaheads(idx:KernelItemIndex) = 
        let ok,v = t.TryGetValue(idx)  
        if ok then v else Set.empty
    member table.Count = t |> Seq.fold(fun acc (KeyValue(_,v)) -> v.Count+acc) 0

/// A mutable table giving an index to each LR(0) kernel. Kernels are referred to only by index.
type KernelTable(kernels) =
    // Give an index to each LR(0) kernel, and from now on refer to them only by index 
    // Also develop "kernelItemIdx" to refer to individual items within a kernel 
    let kernelsAndIdxs = List.mapi (fun i x -> (i,x)) kernels
    let kernelIdxs = List.map fst kernelsAndIdxs
    let toIdxMap = Map.ofList [ for i,x in kernelsAndIdxs -> x,i ]
    let ofIdxMap = Array.ofList kernels
    member t.Indexes = kernelIdxs
    member t.Index(kernel) = toIdxMap.[kernel]
    member t.Kernel(i) = ofIdxMap.[i]

/// Hold the results of cpmuting the LALR(1) closure of an LR(0) kernel
type Closure1Table() = 
    let t = new Dictionary<Item,HashSet<TerminalIndex>>()
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


/// Compile a pre-processed LALR parser spec to tables following the Dragon book algorithm
let CompilerLalrParserSpec logf (newprec:bool) (norec:bool) (spec : ProcessedParserSpec) =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let reportTime() =
        printfn "time: %d(ms)" stopWatch.ElapsedMilliseconds
        stopWatch.Restart()

    // Build indexed tables

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

    let indexOfSymbol (sym : Symbol) =
        match sym with
        | Terminal s -> TerminalIndex (indexOfTerminal.[s])
        | NonTerminal s -> NonTerminalIndex (indexOfNonTerminal.[s])

    let productionsHeads = Array.map (fun p -> indexOfNonTerminal.[p.Head]) spec.Productions
    let productionBodies = Array.map (fun p -> Array.map indexOfSymbol p.Body) spec.Productions
    let productionPrecedences = Array.map (fun p -> p.PrecedenceInfo) spec.Productions
    let productionsOfNonTerminal : ProductionIndex[][] =
        let table = Array.init spec.NonTerminals.Length (fun i -> ResizeArray<int>())
        for i = 0 to spec.Productions.Length - 1 do
            table.[productionsHeads.[i]].Add(i)
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
                let head = productionsHeads.[prodIdx]
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
    let precedenceOfItem (item : Item) = productionPrecedences.[item.ProductionIndex]
    let headOfItem (item : Item) = productionsHeads.[item.ProductionIndex]

    let lsyms_of_item (item : Item) = 
        let syms = productionBodies.[item.ProductionIndex]
        let dotIdx = item.DotIndex
        if dotIdx <= 0 then [||] else syms.[..dotIdx-1]

    let rsyms_of_item (item : Item) = 
        let syms = productionBodies.[item.ProductionIndex]
        let dotIdx = item.DotIndex
        syms.[dotIdx..]

    let rsym_of_item (item : Item) = 
        let prodIdx = item.ProductionIndex
        let dotIdx = item.DotIndex
        let body = productionBodies.[prodIdx]
        if dotIdx < body.Length then
            Some body.[dotIdx]
        elif dotIdx = body.Length then
            None
        else failwith "unreachable"

    let advance_of_item (item : Item) = { item with DotIndex = item.DotIndex + 1 }

    let fakeStartNonTerminalsSet = Set.ofArray (Array.map (fun s -> indexOfNonTerminal.[s]) spec.StartSymbols)

    let IsStartItem (item : Item) = fakeStartNonTerminalsSet.Contains(headOfItem item)
    let IsKernelItem (item : Item) = (IsStartItem item || item.DotIndex <> 0)

    let StringOfSym sym = match sym with TerminalIndex s -> "'" + fst spec.Terminals.[s] + "'" | NonTerminalIndex s -> spec.NonTerminals.[s]

    let OutputSym os sym = fprintf os "%s" (StringOfSym sym)

    let OutputSyms os syms =
        fprintf os "%s" (String.Join(" ",Array.map StringOfSym syms))

    // Print items and other stuff 
    let OutputItem os item =
        fprintf os "    %s -&gt; %a . %a" (spec.NonTerminals.[(headOfItem item)]) (* outputPrecInfo precInfo *) OutputSyms (lsyms_of_item item) OutputSyms (rsyms_of_item item) 
        
    let OutputItemSet os s = 
        Set.iter (fun item -> fprintf os "%a\n" OutputItem item) s

    let OutputFirstSet os m = 
        Set.iter (function None ->  fprintf os "&lt;empty&gt;" | Some x -> fprintf os "  term %s\n" x) m

    let OutputFirstMap os m = 
        Map.iter (fun x y -> fprintf os "first '%a' = \n%a\n" OutputSym x OutputFirstSet y) m

    let OutputAction os m = 
        match m with 
        | Shift n -> fprintf os "  shift <a href=\"#s%d\">%d</a>" n n 
        | Reduce prodIdx ->  fprintf os "  reduce %s --&gt; %a" (spec.NonTerminals.[productionsHeads.[prodIdx]]) OutputSyms (productionBodies.[prodIdx])
        | Error ->  fprintf os "  error"
        | Accept -> fprintf os "  accept" 
    
    let OutputActions os (m : (PrecedenceInfo * Action) array) =
        for i = m.Length - 1 downto 0 do
            let prec, action = m.[i]
            let term = fst spec.Terminals.[i]
            fprintf os "    action '%s' (%a): %a\n" term outputPrecInfo prec OutputAction action

    let OutputActionTable os m = 
        Array.iteri (fun i n -> fprintf os "state %d:\n%a\n" i OutputActions n) m

    let OutputImmediateActions os m = 
        match m with 
        | None -> fprintf os "  &lt;none&gt;"
        | Some a -> OutputAction os a
    
    let OutputGotos os m = 
        Array.iteri (fun ntIdx s -> let nonterm = spec.NonTerminals.[ntIdx] in match s with Some st -> fprintf os "    goto %s: <a href=\"#s%d\">%d</a>\n" nonterm st st | None -> ()) m
    
    let OutputCombined os m = 
        Array.iteri (fun i (a,b,c,d) ->
            fprintf os "<div id=\"s%d\">state %d:</div>\n  items:\n%a\n  actions:\n%a\n  immediate action: %a\n  gotos:\n%a\n" i i OutputItemSet a OutputActions b OutputImmediateActions c OutputGotos d) m
    
    let OutputLalrTables os (prods,states, startStates,actionTable,immediateActionTable,gotoTable,endOfInputTerminalIdx,errorTerminalIdx) = 
        let combined = Array.ofList (List.map2 (fun x (y,(z,w)) -> x,y,z,w) (Array.toList states) (List.zip (Array.toList actionTable) (List.zip (Array.toList immediateActionTable) (Array.toList gotoTable))))
        fprintfn os "------------------------";
        fprintfn os "states = ";
        fprintfn os "";
        fprintfn os "%a" OutputCombined combined;
        fprintfn os "startStates = %s" (String.Join(";", (Array.map string startStates)));
        fprintfn os "------------------------"

    let computeClosure (itemSet : Set<Item>) =
        let mutable accu = itemSet
        let queue = Queue<Item>(itemSet)
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
    
    // Right symbols after closing under epsilon moves
    let RelevantSymbolsOfKernel kernel =
        let kernelClosure0 = computeClosure kernel
        Set.fold (fun acc x -> Option.fold (fun acc x -> Set.add x acc) acc (rsym_of_item x)) Set.empty kernelClosure0 

    // Goto set of a kernel of LR(0) nonTerminals, items etc 
    // Input is kernel, output is kernel
    let ComputeGotosOfKernel iset sym = 
        let isetClosure = computeClosure iset
        let acc = new System.Collections.Generic.List<_>(10)
        isetClosure |> Set.iter (fun item -> 
              match rsym_of_item item with 
              | Some sym2 when sym = sym2 -> acc.Add(advance_of_item item) 
              | _ -> ()) 
        Set.ofSeq acc
    
    // Build the full set of LR(0) kernels 
    reportTime(); printf "building kernels..."; stdout.Flush();
    let startItems = Array.init spec.StartSymbols.Length createItem
    let startKernels = Array.map Set.singleton startItems
    let kernels = 

        /// We use a set-of-sets here. F# sets support structural comparison but at the time of writing
        /// did not structural hashing. 
        let acc = ref Set.empty
        processWorkList (List.ofArray startKernels) (fun addToWorkList kernel -> 
            if not ((!acc).Contains(kernel)) then
                acc := (!acc).Add(kernel);
                for csym in RelevantSymbolsOfKernel kernel do 
                    let gotoKernel = ComputeGotosOfKernel kernel csym 
                    assert (gotoKernel.Count > 0)
                    addToWorkList gotoKernel )
                    
        !acc |> Seq.toList |> List.map (Set.filter IsKernelItem)
    
    reportTime(); printf "building kernel table..."; stdout.Flush();
    // Give an index to each LR(0) kernel, and from now on refer to them only by index 
    let kernelTab = new KernelTable(kernels)
    let startKernelIdxs = Array.map kernelTab.Index startKernels
    let startKernelItemIdxs = Array.map2 (fun kernel item -> { KernelIndex = kernel; Item = item }) startKernelIdxs startItems

    let outputKernelItemIdx os (kernelIdx, item)  =
        fprintf os "kernel %d, item %a" kernelIdx OutputItem item

    /// A cached version of the "goto" computation on LR(0) kernels 
    let gotoKernel (gotoItemIndex : GotoItemIndex) = 
            let gset = ComputeGotosOfKernel (kernelTab.Kernel gotoItemIndex.KernelIndex) gotoItemIndex.SymbolIndex
            if gset.IsEmpty then None else Some (kernelTab.Index gset)

    let gotoKernel = memoize1 gotoKernel

    /// Iterate (iset,sym) pairs such that (gotoKernel kernelIdx sym) is not empty
    let IterateGotosOfKernel kernelIdx f =
        for sym in RelevantSymbolsOfKernel (kernelTab.Kernel kernelIdx) do 
            match gotoKernel { KernelIndex = kernelIdx; SymbolIndex = sym } with 
            | None -> ()
            | Some k -> f sym k

    // This is used to compute the closure of an LALR(1) kernel 
    //
    // For each item [A --> X.BY, a] in I
    //   For each production B -> g in G'
    //     For each terminal b in FIRST(Ya)
    //        such that [B --> .g, b] is not in I do
    //            add [B --> .g, b] to I
    
    let ComputeClosure1 iset = 
        let acc = new Closure1Table()
        processWorkList iset (fun addToWorkList (item, pretokens:Set<TerminalIndex>) ->
            pretokens |> Set.iter (fun pretoken -> 
                if not (acc.Contains(item, pretoken)) then
                    acc.Add(item, pretoken) |> ignore
                    let rsyms = rsyms_of_item item
                    if rsyms.Length > 0 then 
                        match rsyms.[0] with 
                        | (NonTerminalIndex ntB) -> 
                             let firstSet = firstSetOfSymbolString (Array.toList rsyms.[1..]) pretoken
                             for prodIdx in productionsOfNonTerminal.[ntB] do
                                 addToWorkList (createItem prodIdx,firstSet)
                        | TerminalIndex _ -> ()))
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
        let closure1OfItemWithDummy item = ComputeClosure1 [(item, Set.ofList [dummyLookaheadIdx])] 
        let closure1OfItemWithDummy = memoize1 closure1OfItemWithDummy

        let spontaneous = new SpontaneousTable()
        let propagate = new PropagateTable()
        let count = ref 0 

        for kernelIdx in kernelTab.Indexes do
            printf  "."; stdout.Flush();
            //printf  "kernelIdx = %d\n" kernelIdx; stdout.Flush();
            let kernel = kernelTab.Kernel(kernelIdx)
            for item in kernel do  
                let itemIdx = { KernelIndex = kernelIdx; Item = item }
                let jset = closure1OfItemWithDummy item
                //printf  "#jset = %d\n" jset.Count; stdout.Flush();
                for (KeyValue(closureItem, lookaheadTokens)) in jset.IEnumerable do
                    incr count
                    match rsym_of_item closureItem with 
                    | None -> ()
                    | Some rsym ->
                         match gotoKernel { KernelIndex = kernelIdx; SymbolIndex = rsym } with 
                         | None -> ()
                         | Some gotoKernelIdx ->
                              let gotoItem = advance_of_item closureItem
                              let gotoItemIdx = { KernelIndex = gotoKernelIdx; Item = gotoItem }
                              for lookaheadToken in lookaheadTokens do
                                  if lookaheadToken = dummyLookaheadIdx 
                                  then propagate.Add(itemIdx, gotoItemIdx) |> ignore
                                  else spontaneous.Add(gotoItemIdx, lookaheadToken) |> ignore


        //printfn "#kernelIdxs = %d, count = %d" kernelTab.Indexes.Length !count
        spontaneous,
        propagate
   
    //printfn "#spontaneous = %d, #propagate = %d" spontaneous.Count propagate.Count; stdout.Flush();
   
    //exit 0;
    // Repeatedly use the "spontaneous" and "propagate" maps to build the full set 
    // of lookaheads for each LR(0) kernelItem.   
    reportTime(); printf  "building lookahead table..."; stdout.Flush();
    let lookaheadTable = 

        // Seed the table with the startKernelItems and the spontaneous info
        let initialWork =
            [ for idx in startKernelItemIdxs do
                  yield (idx,endOfInputTerminalIdx)
              for (KeyValue(kernelItemIdx,lookaheads)) in spontaneous.IEnumerable do
                  for lookahead in lookaheads do
                      yield (kernelItemIdx,lookahead) ]

        let acc = new LookaheadTable()
        // Compute the closure
        processWorkList 
            initialWork
            (fun queueWork (kernelItemIdx,lookahead) ->
                acc.Add(kernelItemIdx, lookahead)
                for gotoKernelIdx in propagate.[kernelItemIdx] do
                    if not (acc.Contains(gotoKernelIdx,lookahead)) then 
                        queueWork(gotoKernelIdx,lookahead))
        acc

    //printf  "built lookahead table, #lookaheads = %d\n" lookaheadTable.Count; stdout.Flush();

    reportTime(); printf "building action table..."; stdout.Flush();
    let shiftReduceConflicts = ref 0
    let reduceReduceConflicts = ref 0
    let actionTable, immediateActionTable = 

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
                                let nt = productionsHeads.[x]
                                "reduce", productionBodies.[x]
                                |> Array.map StringOfSym
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
                    printf "%s/%s error at state %d on terminal %s between %s and %s - assuming the former because %s\n" a1n a2n kernelIdx (fst spec.Terminals.[termIdx]) astr1 astr2 reason
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
            let kernel = kernelTab.Kernel kernelIdx
            let arr = Array.create spec.Terminals.Length (None, Error)

            //printf  "building lookahead table LR(1) items for kernelIdx %d\n" kernelIdx; stdout.Flush();

            // Compute the LR(1) items based on lookaheads
            let items = 
                 [ for item in kernel do
                     let kernelItemIdx = { KernelIndex = kernelIdx; Item = item }
                     let lookaheads = lookaheadTable.GetLookaheads(kernelItemIdx)
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
                        if not (IsStartItem(item)) then
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

            let immediateAction =
                match Set.toList closure with
                | [item] ->
                    let pItem = item.ProductionIndex
                    match (rsym_of_item item) with 
                    | None when (List.init spec.Terminals.Length id |> List.forall(fun terminalIdx -> arr.[terminalIdx] |> function (_, Reduce pItem) -> true | (_, Error) when not <| norec -> true | _ -> false))
                        -> Some (Reduce pItem)

                    | None when (List.init spec.Terminals.Length id |> List.forall(fun terminalIdx -> arr.[terminalIdx] |> function (_, Accept) -> true | (_, Error) when not <| norec -> true | _ -> false))
                        -> Some Accept

                    | _ -> None
                | _ -> None

            // A -> B C . rules give rise to reductions in favour of errors 
            if not <| norec then
                for item in computeClosure kernel do
                    let prec = precedenceOfItem item
                    match rsym_of_item item with 
                    | None ->
                        for terminalIdx = 0 to spec.Terminals.Length - 1 do
                            if snd(arr.[terminalIdx]) = Error then 
                                let prodIdx = item.ProductionIndex
                                let action = (prec, (if IsStartItem(item) then Accept else Reduce prodIdx))
                                addResolvingPrecedence arr kernelIdx terminalIdx action
                    | _  -> ()

            arr,immediateAction

        let actionInfo = List.map ComputeActions kernelTab.Indexes
        Array.ofList (List.map fst actionInfo),
        Array.ofList (List.map snd actionInfo)

    // The goto table is much simpler - it is based on LR(0) kernels alone. 

    reportTime(); printf  "building goto table..."; stdout.Flush();
    let gotoTable = 
         let gotos kernelIdx = Array.init spec.NonTerminals.Length (fun nt ->  gotoKernel ({ KernelIndex = kernelIdx; SymbolIndex = NonTerminalIndex nt }))
         Array.ofList (List.map gotos kernelTab.Indexes)

    reportTime(); printfn  "returning tables."; stdout.Flush();
    if !shiftReduceConflicts > 0 then printfn  "%d shift/reduce conflicts" !shiftReduceConflicts; stdout.Flush();
    if !reduceReduceConflicts > 0 then printfn  "%d reduce/reduce conflicts" !reduceReduceConflicts; stdout.Flush();
    if !shiftReduceConflicts > 0 || !reduceReduceConflicts > 0 then printfn  "consider setting precedences explicitly using %%left %%right and %%nonassoc on terminals and/or setting explicit precedence on rules using %%prec"

    /// The final results
    let states = kernels |> Array.ofList 
    let prods = Array.map (fun (prod : Production) -> (prod.Head, indexOfNonTerminal.[prod.Head], prod.Body, prod.Code)) spec.Productions

    logf (fun logStream -> 
        printf  "writing tables to log\n"; stdout.Flush();
        OutputLalrTables logStream     (prods, states, startKernelIdxs, actionTable, immediateActionTable, gotoTable, (indexOfTerminal.[endOfInputTerminal]), errorTerminalIdx));

    let states = states |> Array.map (Set.toList >> List.map (fun (item : Item) -> item.ProductionIndex))
    (prods, states, startKernelIdxs, 
     actionTable, immediateActionTable, gotoTable, 
     (indexOfTerminal.[endOfInputTerminal]), 
     errorTerminalIdx, spec.NonTerminals)

  
(* Some examples for testing *)  

(*

let example1 = 
  let e = "E" 
  let t = "Terminal"
  let plus = "+"
  let mul = "*"
  let f = "F"
  let lparen = "("
  let rparen = ")"
  let id = "id"
  
  let terminals = [plus; mul; lparen; rparen; id]
  let nonTerminals = [e; t; f]
  
  let p2 = e, (NonAssoc, ExplicitPrec 1), [NonTerminal e; Terminal plus; NonTerminal t], None
  let p3 = e, (NonAssoc, ExplicitPrec 2), [NonTerminal t], None in  
  let p4 = t, (NonAssoc, ExplicitPrec 3), [NonTerminal t; Terminal mul; NonTerminal f], None
  let p5 = t, (NonAssoc, ExplicitPrec 4), [NonTerminal f], None
  let p6 = f, (NonAssoc, ExplicitPrec  5), [Terminal lparen; NonTerminal e; Terminal rparen], None
  let p7 = f, (NonAssoc, ExplicitPrec 6), [Terminal id], None

  let prods = [p2;p3;p4;p5;p6;p7]
  Spec(terminals,nonTerminals,prods, [e])

let example2 = 
  let prods = [ "S", (NonAssoc, ExplicitPrec 1), [NonTerminal "C";NonTerminal "C"], None; 
                "C", (NonAssoc, ExplicitPrec 2), [Terminal "c";NonTerminal "C"], None ;
                "C", (NonAssoc, ExplicitPrec 3), [Terminal "d"] , None  ]in
  Spec(["c";"d"],["S";"C"],prods, ["S"])

let example3 = 
  let terminals = ["+"; "*"; "("; ")"; "id"]
  let nonTerminals = ["E"; "Terminal"; "E'"; "F"; "Terminal'"]
  let prods = [ "E", (NonAssoc, ExplicitPrec 1), [ NonTerminal "Terminal"; NonTerminal "E'" ], None;
                "E'", (NonAssoc, ExplicitPrec 2), [ Terminal "+"; NonTerminal "Terminal"; NonTerminal "E'"], None;
                "E'", (NonAssoc, ExplicitPrec 3), [ ], None;
                "Terminal", (NonAssoc, ExplicitPrec 4), [ NonTerminal "F"; NonTerminal "Terminal'" ], None;
                "Terminal'", (NonAssoc, ExplicitPrec 5), [ Terminal "*"; NonTerminal "F"; NonTerminal "Terminal'"], None;
                "Terminal'", (NonAssoc, ExplicitPrec 6), [ ], None;
                "F", (NonAssoc, ExplicitPrec 7), [ Terminal "("; NonTerminal "E"; Terminal ")"], None;
                "F", (NonAssoc, ExplicitPrec 8), [ Terminal "id"], None ]
  Spec(terminals,nonTerminals,prods, ["E"])

let example4 = 
  let terminals = ["+"; "*"; "("; ")"; "id"]
  let nonTerminals = ["E"]
  let prods = [ "E", (NonAssoc, ExplicitPrec 1), [ NonTerminal "E"; Terminal "+"; NonTerminal "E" ], None;
                "E", (NonAssoc, ExplicitPrec 2), [ NonTerminal "E"; Terminal "*"; NonTerminal "E" ], None;
                "E", (NonAssoc, ExplicitPrec 3), [ Terminal "("; NonTerminal "E"; Terminal ")"], None;
                "E", (NonAssoc, ExplicitPrec 8), [ Terminal "id"],  None ]
  Spec(terminals,nonTerminals,prods, ["E"])

let example5 = 
  let terminals = ["+"; "*"; "("; ")"; "id"]
  let nonTerminals = ["E"]
  let prods = [ "E", (NonAssoc, ExplicitPrec 1), [ NonTerminal "E"; Terminal "+"; NonTerminal "E" ], None;
                "E", (NonAssoc, ExplicitPrec 2), [ NonTerminal "E"; Terminal "*"; NonTerminal "E" ], None;
                "E", (NonAssoc, ExplicitPrec 3), [ Terminal "("; NonTerminal "E"; Terminal ")"], None;
                "E", (NonAssoc, ExplicitPrec 8), [ Terminal "id"], None ]
  Spec(terminals,nonTerminals,prods, ["E"])

let example6 = 
  let terminals = ["+"; "*"; "("; ")"; "id"; "-"]
  let nonTerminals = ["E"]
  let prods = [ "E", (RightAssoc, ExplicitPrec 1), [ NonTerminal "E"; Terminal "-"; NonTerminal "E" ], None;
                "E", (LeftAssoc, ExplicitPrec 1), [ NonTerminal "E"; Terminal "+"; NonTerminal "E" ], None;
                "E", (LeftAssoc, ExplicitPrec 2), [ NonTerminal "E"; Terminal "*"; NonTerminal "E" ], None;
                "E", (NonAssoc, ExplicitPrec 3), [ Terminal "("; NonTerminal "E"; Terminal ")"], None;
                "E", (NonAssoc, ExplicitPrec 8), [ Terminal "id"], None ]
  Spec(terminals,nonTerminals,prods, ["E"])


let example7 = 
  let prods = [ "S", (NonAssoc, ExplicitPrec 1), [NonTerminal "L";Terminal "="; NonTerminal "R"], None; 
                "S", (NonAssoc, ExplicitPrec 2), [NonTerminal "R"], None ;
                "L", (NonAssoc, ExplicitPrec 3), [Terminal "*"; NonTerminal "R"], None;
                "L", (NonAssoc, ExplicitPrec 3), [Terminal "id"], None; 
                "R", (NonAssoc, ExplicitPrec 3), [NonTerminal "L"], None; ]
  Spec(["*";"=";"id"],["S";"L";"R"],prods, ["S"])



let test ex = CompilerLalrParserSpec stdout ex

(* let _ = test example2*)
(* let _ = exit 1*)
(* let _ = test example3 
let _ = test example1  
let _ = test example4
let _ = test example5
let _ = test example6 *)
*)
