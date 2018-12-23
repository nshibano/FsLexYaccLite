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
      Body : Symbol list
      PrecedenceInfo : PrecedenceInfo
      Code : Code option }

type ProcessedParserSpec = 
    { Terminals: (string * PrecedenceInfo) list
      NonTerminals: string list
      Productions: Production list
      StartSymbols: string list }

let processParserSpecAst (spec : ParserSpec) =
    
    let explicitPrecInfo = 
        let levels = spec.Associativities.Length
        spec.Associativities 
        |> List.mapi (fun i precSpecs -> List.map (fun (precSym, assoc) -> (precSym, (assoc, levels - i - 1))) precSpecs)
        |> List.concat
    
    for key, _ in explicitPrecInfo |> Seq.countBy fst |> Seq.filter (fun (_, n) -> n > 1)  do
        failwithf "%s is given two associativities" key
    
    let explicitPrecInfo = Map.ofList explicitPrecInfo
    let terminals = List.map fst spec.Tokens @ ["error"]
    let terminalSet = Set.ofList terminals
       
    let prods =  
        spec.Rules |> List.mapi (fun i (nonterm,rules) -> 
            rules |> List.mapi (fun j (Rule(syms,precsym,code)) -> 
                let precInfo = 
                    let precsym = List.foldBack (fun x acc -> match acc with Some _ -> acc | None -> match x with z when terminalSet.Contains z -> Some z | _ -> acc) syms precsym
                    match precsym with 
                    | Some sym -> if explicitPrecInfo.ContainsKey(sym) then Some explicitPrecInfo.[sym] else None
                    | None -> None
                { Head = nonterm; PrecedenceInfo = precInfo; Body = List.map (fun s -> if terminalSet.Contains s then Terminal s else NonTerminal s) syms; Code = code }))
         |> List.concat

    let nonTerminals = List.map fst spec.Rules
    let nonTerminalSet = Set.ofList nonTerminals

    let checkNonTerminal nt =  
        if nt <> "error" && not (nonTerminalSet.Contains(nt)) then 
            failwith (sprintf "NonTerminal '%s' has no productions" nt)

    for prod in prods do
        for sym in prod.Body do 
           match sym with 
           | NonTerminal nt -> checkNonTerminal nt 
           | Terminal t -> if not (terminalSet.Contains t) then failwith (sprintf "token %s is not declared" t)
           
    if spec.StartSymbols = [] then (failwith "at least one %start declaration is required\n")

    for nt,_ in spec.Types do 
        checkNonTerminal nt

    let terminals = List.map (fun t -> (t, explicitPrecInfo.TryFind t)) terminals

    { Terminals = terminals
      NonTerminals = nonTerminals
      Productions = prods
      StartSymbols = spec.StartSymbols }

//-------------------------------------------------
// Process LALR(1) grammars to tables

/// Represent (ProductionIndex,ProdictionDotIndex) as one integer 
type Item0 = //uint32
    { ProductionIndex : int
      DotIndex : int }

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

/// LR(0) kernels
type Kernel = Set<Item0>

/// Indexes of LR(0) kernels in the KernelTable
type KernelIdx = int

/// Indexes in the TerminalTable and NonTerminalTable
type TerminalIndex = int
type NonTerminalIndex = int

/// Representation of Symbols
type SymbolIndex =
    | NonTerminalIndex of int
    | TerminalIndex of int

/// Indexes in the LookaheadTable, SpontaneousTable, PropagateTable
/// Embed in a single integer, since these are faster
/// keys for the dictionary hash tables
///
/// Logically:
///
///   type KernelItemIndex = KernelItemIdx of KernelIdx * Item0
type KernelItemIndex = { KernelIndex : int; Item0 : Item0 }
let KernelItemIdx (i1,i2) = { KernelIndex = i1; Item0 = i2 }

/// Indexes into the memoizing table for the Goto computations
/// Embed in a single integer, since these are faster
/// keys for the dictionary hash tables
///
/// Logically:
///
///   type GotoItemIndex = GotoItemIdx of KernelIdx * SymbolIndex
type GotoItemIndex =
    { KernelIndex : int
      SymbolIndex : SymbolIndex } //uint64
let GotoItemIdx (i1:KernelIdx,i2:SymbolIndex) =
    { KernelIndex = i1
      SymbolIndex = i2 }
    ////(uint64 (uint32 i1) <<< 32) ||| uint64 (uint32 i2)
let (|GotoItemIdx|) (i64:GotoItemIndex) =
    (i64.KernelIndex, i64.SymbolIndex) //int32 ((i64 >>> 32) &&& 0xFFFFFFFFUL), int32 (i64 &&& 0xFFFFFFFFUL)

/// Create a work list and loop until it is exhausted, calling a worker function for
/// each element. Pass a function to queue additional work on the work list 
/// to the worker function
let ProcessWorkList start f =
    let work = ref (start : 'a list)
    let queueWork = (fun x -> work := x :: !work)
    let rec loop() = 
        match !work with 
        | [] -> ()
        | x::t -> 
            work := t; 
            f queueWork x;
            loop()
    loop()

/// A standard utility to compute a least fixed point of a set under a generative computation
let LeastFixedPoint f set = 
    let acc = ref set
    ProcessWorkList (Set.toList set) (fun queueWork item ->
          f(item) |> List.iter (fun i2 -> if not (Set.contains i2 !acc) then (acc := Set.add i2 !acc; queueWork i2)) )
    !acc

/// A general standard memoization utility. Be sure to apply to only one (function) argument to build the
/// residue function!
let Memoize f = 
    let t = new Dictionary<_,_>()
    fun x -> 
        let ok,v = t.TryGetValue(x) 
        if ok then v else let res = f x in t.[x] <- res; res 

/// A mutable table maping kernels to sets of lookahead tokens
type LookaheadTable() = 
    let t = new Dictionary<KernelItemIndex,Set<TerminalIndex>>()
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
    let t = new Dictionary<Item0,HashSet<TerminalIndex>>()
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

    // Augment the grammar 
    let fakeStartNonTerminals = List.map (fun nt -> "_start" + nt) spec.StartSymbols
    let nonTerminals = Array.ofList (fakeStartNonTerminals @ spec.NonTerminals)
    let endOfInputTerminal = "$$"
    let dummyLookahead = "#"
    let terminals = Array.ofList (spec.Terminals @ [(dummyLookahead, None); (endOfInputTerminal, None)])
    let productions = Array.ofList (List.map2 (fun fakeStartNonTerminal startSymbol -> { Head = fakeStartNonTerminal; PrecedenceInfo = None; Body = [NonTerminal startSymbol]; Code = None }) fakeStartNonTerminals spec.StartSymbols @ spec.Productions)
    let startNonTerminalIdx_to_prodIdx (i : int) = i

    // Build indexed tables

    let indexOfNonTerminal =
        let d = Dictionary<string, int>()
        for i = 0 to nonTerminals.Length - 1 do
            d.Add(nonTerminals.[i], i)
        d

    let indexOfTerminal =
        let d = Dictionary<string, int>()
        for i = 0 to terminals.Length - 1 do
            d.Add(fst terminals.[i], i)
        d

    let indexOfSymbol (sym : Symbol) =
        match sym with
        | Terminal s -> TerminalIndex (indexOfTerminal.[s])
        | NonTerminal s -> NonTerminalIndex (indexOfNonTerminal.[s])

    let productionsHeads = Array.map (fun p -> indexOfNonTerminal.[p.Head]) productions
    let productionBodies = Array.map (fun p -> Array.map indexOfSymbol (Array.ofList p.Body)) productions
    let productionPrecedences = Array.map (fun p -> p.PrecedenceInfo) productions
    let productionsOfNonTerminal =
        let table = Array.init nonTerminals.Length (fun i -> ResizeArray<int>())
        for i = 0 to productions.Length - 1 do
            table.[productionsHeads.[i]].Add(i)
        Array.map (fun (l : ResizeArray<int>) -> l.ToArray()) table

    let dummyLookaheadIdx = indexOfTerminal.[dummyLookahead]
    let endOfInputTerminalIdx = indexOfTerminal.[endOfInputTerminal]
    let errorTerminalIdx = indexOfTerminal.["error"]

    // Compute the FIRST function
    printf  "computing first function..."; stdout.Flush();

    let computedFirstTable =
        let mutable firstSets = Map.empty<SymbolIndex, Set<TerminalIndex option>>
        
        // For terminals, add itself (Some term) to its first-set.
        for term = 0 to terminals.Length - 1 do
            firstSets <- Map.add (TerminalIndex term) (Set.singleton (Some term)) firstSets

        // For non-terminals, start with empty set.
        for nonTerm = 0 to nonTerminals.Length - 1 do
            firstSets <- Map.add (NonTerminalIndex nonTerm) Set.empty firstSets
        
        let add symbolIndex firstSetItem = 
            let set = firstSets.[symbolIndex]
            if not (Set.contains firstSetItem set) then 
                firstSets <- Map.add symbolIndex (Set.add firstSetItem set) firstSets

        let scan() =
            for prodIdx = 0 to productions.Length - 1 do
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
                
        let mutable cont = true
        while cont do
            let atStart = firstSets
            scan()
            if LanguagePrimitives.PhysicalEquality firstSets atStart then
                cont <- false
        
        firstSets

    /// Compute the first set of the given sequence of non-terminals. If any of the non-terminals
    /// have an empty token in the first set then we have to iterate through those. 
    let ComputeFirstSetOfTokenList =
        Memoize (fun (str : SymbolIndex list, term : TerminalIndex) ->
            let acc = List<TerminalIndex>()
            let rec add l = 
                match l with 
                | [] -> acc.Add(term)
                | sym :: moreSyms -> 
                    let firstSetOfSym = computedFirstTable.[sym]
                    Set.iter (function Some v -> acc.Add(v) | None -> ()) firstSetOfSym
                    if firstSetOfSym.Contains(None) then
                        add moreSyms 
            add str
            Set.ofSeq acc)
    
    // (int,int) representation of LR(0) items 
    let prodIdx_to_item0 idx = { ProductionIndex = idx; DotIndex = 0 }
    let prec_of_item0 (item0 : Item0) = productionPrecedences.[item0.ProductionIndex]
    let ntIdx_of_item0 (item0 : Item0) = productionsHeads.[item0.ProductionIndex]

    let lsyms_of_item0 (item0 : Item0) = 
        let prodIdx = item0.ProductionIndex
        let dotIdx = item0.DotIndex
        let syms = productionBodies.[prodIdx]
        if dotIdx <= 0 then [||] else syms.[..dotIdx-1]

    let rsyms_of_item0 (item0 : Item0) = 
        let prodIdx = item0.ProductionIndex
        let dotIdx = item0.DotIndex
        let syms = productionBodies.[prodIdx]
        syms.[dotIdx..]

    let rsym_of_item0 (item0 : Item0) = 
        let prodIdx = item0.ProductionIndex
        let dotIdx = item0.DotIndex
        let body = productionBodies.[prodIdx]
        if dotIdx < body.Length then
            Some body.[dotIdx]
        elif dotIdx = body.Length then
            None
        else failwith "unreachable"

    let advance_of_item0 (item0 : Item0) = { item0 with DotIndex = item0.DotIndex + 1 }

    let fakeStartNonTerminalsSet = Set.ofList (fakeStartNonTerminals |> List.map (fun s -> indexOfNonTerminal.[s]))

    let IsStartItem (item0 : Item0) = fakeStartNonTerminalsSet.Contains(ntIdx_of_item0 item0)
    let IsKernelItem (item0 : Item0) = (IsStartItem item0 || item0.DotIndex <> 0)

    let StringOfSym sym = match sym with TerminalIndex s -> "'" + fst terminals.[s] + "'" | NonTerminalIndex s -> nonTerminals.[s]

    let OutputSym os sym = fprintf os "%s" (StringOfSym sym)

    let OutputSyms os syms =
        fprintf os "%s" (String.Join(" ",Array.map StringOfSym syms))

    // Print items and other stuff 
    let OutputItem0 os item0 =
        fprintf os "    %s -&gt; %a . %a" (nonTerminals.[(ntIdx_of_item0 item0)]) (* outputPrecInfo precInfo *) OutputSyms (lsyms_of_item0 item0) OutputSyms (rsyms_of_item0 item0) 
        
    let OutputItem0Set os s = 
        Set.iter (fun item -> fprintf os "%a\n" OutputItem0 item) s

    let OutputFirstSet os m = 
        Set.iter (function None ->  fprintf os "&lt;empty&gt;" | Some x -> fprintf os "  term %s\n" x) m

    let OutputFirstMap os m = 
        Map.iter (fun x y -> fprintf os "first '%a' = \n%a\n" OutputSym x OutputFirstSet y) m

    let OutputAction os m = 
        match m with 
        | Shift n -> fprintf os "  shift <a href=\"#s%d\">%d</a>" n n 
        | Reduce prodIdx ->  fprintf os "  reduce %s --&gt; %a" (nonTerminals.[productionsHeads.[prodIdx]]) OutputSyms (productionBodies.[prodIdx])
        | Error ->  fprintf os "  error"
        | Accept -> fprintf os "  accept" 
    
    let OutputActions os (m : (PrecedenceInfo * Action) array) =
        for i = m.Length - 1 downto 0 do
            let prec, action = m.[i]
            let term = fst terminals.[i]
            fprintf os "    action '%s' (%a): %a\n" term outputPrecInfo prec OutputAction action

    let OutputActionTable os m = 
        Array.iteri (fun i n -> fprintf os "state %d:\n%a\n" i OutputActions n) m

    let OutputImmediateActions os m = 
        match m with 
        | None -> fprintf os "  &lt;none&gt;"
        | Some a -> OutputAction os a
    
    let OutputGotos os m = 
        Array.iteri (fun ntIdx s -> let nonterm = nonTerminals.[ntIdx] in match s with Some st -> fprintf os "    goto %s: <a href=\"#s%d\">%d</a>\n" nonterm st st | None -> ()) m
    
    let OutputCombined os m = 
        Array.iteri (fun i (a,b,c,d) ->
            fprintf os "<div id=\"s%d\">state %d:</div>\n  items:\n%a\n  actions:\n%a\n  immediate action: %a\n  gotos:\n%a\n" i i OutputItem0Set a OutputActions b OutputImmediateActions c OutputGotos d) m
    
    let OutputLalrTables os (prods,states, startStates,actionTable,immediateActionTable,gotoTable,endOfInputTerminalIdx,errorTerminalIdx) = 
        let combined = Array.ofList (List.map2 (fun x (y,(z,w)) -> x,y,z,w) (Array.toList states) (List.zip (Array.toList actionTable) (List.zip (Array.toList immediateActionTable) (Array.toList gotoTable))))
        fprintfn os "------------------------";
        fprintfn os "states = ";
        fprintfn os "";
        fprintfn os "%a" OutputCombined combined;
        fprintfn os "startStates = %s" (String.Join(";",Array.ofList (List.map string startStates)));
        fprintfn os "------------------------"


    // Closure of LR(0) nonTerminals, items etc 
    let ComputeClosure0NonTerminal = 
        Memoize (fun nt -> 
            let seed = (Array.foldBack (prodIdx_to_item0 >> Set.add) productionsOfNonTerminal.[nt] Set.empty)
            LeastFixedPoint 
                (fun item0 -> 
                   match rsym_of_item0 item0 with
                   | None -> []
                   | Some(NonTerminalIndex ntB) ->  List.ofArray (Array.map prodIdx_to_item0 productionsOfNonTerminal.[ntB])
                   | Some(TerminalIndex _) -> [])
                seed)

    // Close a symbol under epsilon moves
    let ComputeClosure0Symbol rsym acc = 
        match rsym with
        | Some (NonTerminalIndex nt) -> Set.union (ComputeClosure0NonTerminal nt) acc
        | _ -> acc

    // Close a set under epsilon moves
    let ComputeClosure0 iset = 
        Set.fold (fun acc x -> ComputeClosure0Symbol (rsym_of_item0 x) acc) iset iset 

    // Right symbols after closing under epsilon moves
    let RelevantSymbolsOfKernel kernel =
        let kernelClosure0 = ComputeClosure0 kernel
        Set.fold (fun acc x -> Option.fold (fun acc x -> Set.add x acc) acc (rsym_of_item0 x)) Set.empty kernelClosure0 

    // Goto set of a kernel of LR(0) nonTerminals, items etc 
    // Input is kernel, output is kernel
    let ComputeGotosOfKernel iset sym = 
        let isetClosure = ComputeClosure0 iset
        let acc = new System.Collections.Generic.List<_>(10)
        isetClosure |> Set.iter (fun item0 -> 
              match rsym_of_item0 item0 with 
              | Some sym2 when sym = sym2 -> acc.Add(advance_of_item0 item0) 
              | _ -> ()) 
        Set.ofSeq acc
    
    // Build the full set of LR(0) kernels 
    reportTime(); printf "building kernels..."; stdout.Flush();
    let startItems = List.mapi (fun i _ -> prodIdx_to_item0 (startNonTerminalIdx_to_prodIdx i)) fakeStartNonTerminals
    let startKernels = List.map Set.singleton startItems
    let kernels = 

        /// We use a set-of-sets here. F# sets support structural comparison but at the time of writing
        /// did not structural hashing. 
        let acc = ref Set.empty
        ProcessWorkList startKernels (fun addToWorkList kernel -> 
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
    let startKernelIdxs = List.map kernelTab.Index startKernels
    let startKernelItemIdxs = List.map2 (fun a b -> KernelItemIdx(a,b)) startKernelIdxs startItems

    let outputKernelItemIdx os (kernelIdx,item0)  =
        fprintf os "kernel %d, item %a" kernelIdx OutputItem0 item0

    /// A cached version of the "goto" computation on LR(0) kernels 
    let gotoKernel = 
        Memoize (fun (GotoItemIdx(kernelIdx,sym)) -> 
            let gset = ComputeGotosOfKernel (kernelTab.Kernel kernelIdx) sym
            if gset.IsEmpty then None else Some (kernelTab.Index gset))

    /// Iterate (iset,sym) pairs such that (gotoKernel kernelIdx sym) is not empty
    let IterateGotosOfKernel kernelIdx f =
        for sym in RelevantSymbolsOfKernel (kernelTab.Kernel kernelIdx) do 
            match gotoKernel (GotoItemIdx(kernelIdx,sym)) with 
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
        ProcessWorkList iset (fun addToWorkList (item0,pretokens:Set<TerminalIndex>) ->
            pretokens |> Set.iter (fun pretoken -> 
                if not (acc.Contains(item0,pretoken)) then
                    acc.Add(item0,pretoken) |> ignore
                    let rsyms = rsyms_of_item0 item0 
                    if rsyms.Length > 0 then 
                        match rsyms.[0] with 
                        | (NonTerminalIndex ntB) -> 
                             let firstSet = ComputeFirstSetOfTokenList (Array.toList rsyms.[1..],pretoken)
                             for prodIdx in productionsOfNonTerminal.[ntB] do
                                 addToWorkList (prodIdx_to_item0 prodIdx,firstSet)
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
        let closure1OfItem0WithDummy = 
            Memoize (fun item0 -> ComputeClosure1 [(item0,Set.ofList [dummyLookaheadIdx])])

        let spontaneous = new SpontaneousTable()
        let propagate = new PropagateTable()
        let count = ref 0 

        for kernelIdx in kernelTab.Indexes do
            printf  "."; stdout.Flush();
            //printf  "kernelIdx = %d\n" kernelIdx; stdout.Flush();
            let kernel = kernelTab.Kernel(kernelIdx)
            for item0 in kernel do  
                let item0Idx = KernelItemIdx(kernelIdx,item0)
                let jset = closure1OfItem0WithDummy item0
                //printf  "#jset = %d\n" jset.Count; stdout.Flush();
                for (KeyValue(closureItem0, lookaheadTokens)) in jset.IEnumerable do
                    incr count
                    match rsym_of_item0 closureItem0 with 
                    | None -> ()
                    | Some rsym ->
                         match gotoKernel (GotoItemIdx(kernelIdx,rsym)) with 
                         | None -> ()
                         | Some gotoKernelIdx ->
                              let gotoItem = advance_of_item0 closureItem0
                              let gotoItemIdx = KernelItemIdx(gotoKernelIdx,gotoItem)
                              for lookaheadToken in lookaheadTokens do
                                  if lookaheadToken = dummyLookaheadIdx 
                                  then propagate.Add(item0Idx, gotoItemIdx) |> ignore
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
        ProcessWorkList 
            initialWork
            (fun queueWork (kernelItemIdx,lookahead) ->
                acc.Add(kernelItemIdx,lookahead)
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
                                |> sprintf "reduce(%s:%s)" (nonTerminals.[nt])
                            | _ -> "", ""
                        let pstr = 
                            match p with 
                            | Some (assoc, n) -> sprintf "[explicit %s %d]" (stringOfAssoc assoc) n
                            | None  -> "noprec"
                        an, "{" + pstr + " " + astr + "}"
                    let a1n, astr1 = reportAction x1
                    let a2n, astr2 = reportAction x2
                    printf "%s/%s error at state %d on terminal %s between %s and %s - assuming the former because %s\n" a1n a2n kernelIdx (fst terminals.[termIdx]) astr1 astr2 reason
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
            let arr = Array.create terminals.Length (None, Error)

            //printf  "building lookahead table LR(1) items for kernelIdx %d\n" kernelIdx; stdout.Flush();

            // Compute the LR(1) items based on lookaheads
            let items = 
                 [ for item0 in kernel do
                     let kernelItemIdx = KernelItemIdx(kernelIdx,item0)
                     let lookaheads = lookaheadTable.GetLookaheads(kernelItemIdx)
                     yield (item0,lookaheads) ]
                 |> ComputeClosure1

            for (KeyValue(item0,lookaheads)) in items.IEnumerable do

                let nonTermA = ntIdx_of_item0 item0
                match rsym_of_item0 item0 with 
                | Some (TerminalIndex termIdx) -> 
                    let action =
                      match gotoKernel (GotoItemIdx(kernelIdx, TerminalIndex termIdx)) with 
                      | None -> failwith "action on terminal should have found a non-empty goto state"
                      | Some gkernelItemIdx -> Shift gkernelItemIdx
                    let prec = snd terminals.[termIdx]
                    addResolvingPrecedence arr kernelIdx termIdx (prec, action) 
                | None ->
                    for lookahead in lookaheads do
                        if not (IsStartItem(item0)) then
                            let prodIdx = item0.ProductionIndex
                            let prec = prec_of_item0 item0
                            let action = (prec, Reduce prodIdx)
                            addResolvingPrecedence arr kernelIdx lookahead action 
                        elif lookahead = endOfInputTerminalIdx then
                            let prec = prec_of_item0 item0
                            let action = (prec,Accept)
                            addResolvingPrecedence arr kernelIdx lookahead action 
                        else ()
                | _ -> ()

            // If there is a single item A -> B C . and no Shift or Accept actions (i.e. only Error or Reduce, so the choice of terminal 
            // cannot affect what we do) then we emit an immediate reduce action for the rule corresponding to that item 
            // Also do the same for Accept rules. 
            let closure = (ComputeClosure0 kernel)

            let immediateAction =
                match Set.toList closure with
                | [item0] ->
                    let pItem0 = item0.ProductionIndex
                    match (rsym_of_item0 item0) with 
                    | None when (List.init terminals.Length id |> List.forall(fun terminalIdx -> arr.[terminalIdx] |> function (_, Reduce pItem0) -> true | (_, Error) when not <| norec -> true | _ -> false))
                        -> Some (Reduce pItem0)

                    | None when (List.init terminals.Length id |> List.forall(fun terminalIdx -> arr.[terminalIdx] |> function (_, Accept) -> true | (_, Error) when not <| norec -> true | _ -> false))
                        -> Some Accept

                    | _ -> None
                | _ -> None

            // A -> B C . rules give rise to reductions in favour of errors 
            if not <| norec then
                for item0 in ComputeClosure0 kernel do
                    let prec = prec_of_item0 item0
                    match rsym_of_item0 item0 with 
                    | None ->
                        for terminalIdx = 0 to terminals.Length - 1 do
                            if snd(arr.[terminalIdx]) = Error then 
                                let prodIdx = item0.ProductionIndex
                                let action = (prec, (if IsStartItem(item0) then Accept else Reduce prodIdx))
                                addResolvingPrecedence arr kernelIdx terminalIdx action
                    | _  -> ()

            arr,immediateAction

        let actionInfo = List.map ComputeActions kernelTab.Indexes
        Array.ofList (List.map fst actionInfo),
        Array.ofList (List.map snd actionInfo)

    // The goto table is much simpler - it is based on LR(0) kernels alone. 

    reportTime(); printf  "building goto table..."; stdout.Flush();
    let gotoTable = 
         let gotos kernelIdx = Array.init nonTerminals.Length (fun nt ->  gotoKernel (GotoItemIdx(kernelIdx, NonTerminalIndex nt)))
         Array.ofList (List.map gotos kernelTab.Indexes)

    reportTime(); printfn  "returning tables."; stdout.Flush();
    if !shiftReduceConflicts > 0 then printfn  "%d shift/reduce conflicts" !shiftReduceConflicts; stdout.Flush();
    if !reduceReduceConflicts > 0 then printfn  "%d reduce/reduce conflicts" !reduceReduceConflicts; stdout.Flush();
    if !shiftReduceConflicts > 0 || !reduceReduceConflicts > 0 then printfn  "consider setting precedences explicitly using %%left %%right and %%nonassoc on terminals and/or setting explicit precedence on rules using %%prec"

    /// The final results
    let states = kernels |> Array.ofList 
    let prods = Array.map (fun (prod : Production) -> (prod.Head, indexOfNonTerminal.[prod.Head], prod.Body, prod.Code)) productions

    logf (fun logStream -> 
        printf  "writing tables to log\n"; stdout.Flush();
        OutputLalrTables logStream     (prods, states, startKernelIdxs, actionTable, immediateActionTable, gotoTable, (indexOfTerminal.[endOfInputTerminal]), errorTerminalIdx));

    let states = states |> Array.map (Set.toList >> List.map (fun (item0 : Item0) -> item0.ProductionIndex))
    (prods, states, startKernelIdxs, 
     actionTable, immediateActionTable, gotoTable, 
     (indexOfTerminal.[endOfInputTerminal]), 
     errorTerminalIdx, nonTerminals)

  
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
