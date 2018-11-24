(* (c) Microsoft Corporation 2005-2008.  *)

module FsLexYaccLite.Lex.AST

open System
open System.Collections.Generic

open FsLexYaccLite.Lex.Syntax

type MultiMap<'a, 'b> = Dictionary<'a, List<'b>>

let createMultiMap() : MultiMap<_, _> = Dictionary()

let AddToMultiMap (dict : MultiMap<_,_>) a b =
    match dict.TryGetValue(a) with
    | true, l -> l.Add(b)
    | false, _ ->
        let l = List()
        l.Add(b)
        dict.[a] <- l

type NfaNode = 
    { Id : int
      Transitions : MultiMap<Alphabet, NfaNode>
      Accepted: (int * int) option }

type NfaNodeMap = List<NfaNode>

let [<Literal>] Epsilon = -1

let NfaOfRule (macros: Dictionary<string, Regexp>) (clauses: Clause list) = 

    /// Table allocating node ids 
    let nfaNodeMap = new NfaNodeMap()
    
    let newNfaNode trs ac =
        let nodeId = nfaNodeMap.Count
        let trDict = createMultiMap()
        for (a, b) in trs do
            AddToMultiMap trDict a b
        let node = { Id = nodeId; Transitions = trDict; Accepted = ac }
        nfaNodeMap.Add node
        node

    /// Compile a regular expression into the NFA
    let rec CompileRegexp re dest = 
        match re with 
        | Alt res -> 
            let trs = List.map (fun re -> (Epsilon, CompileRegexp re dest)) res
            newNfaNode trs None
        | Seq res -> 
            List.foldBack (CompileRegexp) res dest
        | Inp (Alphabet c) -> 
            newNfaNode [(c, dest)] None
        | Star re -> 
            let nfaNode = newNfaNode [(Epsilon, dest)] None
            let sre = CompileRegexp re nfaNode
            AddToMultiMap nfaNode.Transitions Epsilon sre
            newNfaNode [(Epsilon,sre); (Epsilon,dest)] None
        | Macro m ->
            match macros.TryGetValue(m) with
            | true, re -> CompileRegexp re dest
            | false, _ -> failwith (sprintf "The macro %s is not defined" m)
        | _ -> failwith "dontcare"

    let actions = new System.Collections.Generic.List<_>()
    
    /// Compile an acceptance of a regular expression into the NFA
    let sTrans nodeId (regexp, code) = 
        let actionId = actions.Count
        actions.Add(code)
        let sAccept = newNfaNode [] (Some (nodeId, actionId))
        CompileRegexp regexp sAccept 

    let trs = List.mapi (fun n x -> (Epsilon, sTrans n x)) clauses
    let nfaStartNode = newNfaNode trs None
    nfaStartNode, actions, nfaNodeMap

type NfaNodeIdSetBuilder = HashSet<int>
let createNfaNodeIdSetBuilder() : NfaNodeIdSetBuilder = HashSet<int>()

type NfaNodeIdSet = int array
let createNfaNodeIdSet (builder : NfaNodeIdSetBuilder) : NfaNodeIdSet =
    let ary = Array.zeroCreate<int> builder.Count
    builder.CopyTo(ary)
    Array.sortInPlace ary
    ary

let newDfaNodeId = 
    let i = ref 0 
    fun () -> let res = !i in incr i; res

type DfaNode = 
    { Id: int
      Transitions : List<Alphabet * DfaNode>
      Accepted: (int * int) array }

let NfaToDfa (nfaNodeMap : NfaNodeMap) nfaStartNode = 

    let rec EClosure1 (acc : NfaNodeIdSetBuilder) (n : NfaNode) =
        if acc.Add(n.Id) then
            match n.Transitions.TryGetValue(Epsilon) with
            | true, l ->
                for n in l do
                    EClosure1 acc n
            | false, _ -> ()

    let EClosure (moves : List<int>) = 
        let acc = createNfaNodeIdSetBuilder()
        for i in moves do
            EClosure1 acc nfaNodeMap.[i]
        createNfaNodeIdSet acc

    // Compute all the immediate one-step moves for a set of NFA states, as a dictionary
    // mapping inputs to destination lists
    let ComputeMoves (nset : NfaNodeIdSet) = 
        let moves = createMultiMap()
        for nid in nset do
            for KeyValue(inp, dests) in nfaNodeMap.[nid].Transitions do
                if inp <> Epsilon then
                    for dest in dests do
                        AddToMultiMap moves inp dest.Id
        moves

    let nfaSet0 =
        let acc = createNfaNodeIdSetBuilder()
        EClosure1 acc nfaStartNode
        createNfaNodeIdSet acc

    let dfaNodes = Dictionary<NfaNodeIdSet, DfaNode>(LanguagePrimitives.FastGenericEqualityComparer)
    
    let GetDfaNode nfaSet =
        match dfaNodes.TryGetValue(nfaSet) with
        | true, dfaNode -> dfaNode
        | false, _ ->
            let dfaNode =
                { Id = newDfaNodeId()
                  Transitions = List()
                  Accepted =
                    let accu = List()
                    for nid in nfaSet do
                        let nfaNode = nfaNodeMap.[nid]
                        match nfaNode.Accepted with
                        | Some acc -> accu.Add(acc)
                        | None -> ()
                    accu.ToArray() }
            dfaNodes.Add(nfaSet, dfaNode)
            dfaNode
            
    let workList = Queue([| nfaSet0 |])
    let doneSet = HashSet<NfaNodeIdSet>(LanguagePrimitives.FastGenericEqualityComparer)

    while workList.Count > 0 do
        let nfaSet = workList.Dequeue()
        if doneSet.Add(nfaSet) then
            let moves = ComputeMoves nfaSet
            for (KeyValue(inp, movesForInput)) in moves do
                assert (inp <> Epsilon)
                let moveSet = EClosure movesForInput
                if moveSet.Length > 0 then 
                    let dfaNode = GetDfaNode nfaSet
                    dfaNode.Transitions.Add((inp, GetDfaNode moveSet))
                    workList.Enqueue(moveSet)

    let ruleStartNode = GetDfaNode nfaSet0
    let ruleNodes =
        let dfaNodes = Array.ofSeq dfaNodes.Values
        Array.sortInPlaceBy (fun (s : DfaNode) -> s.Id) dfaNodes
        List.ofArray dfaNodes
    ruleStartNode, ruleNodes

let Compile spec =
    let alphabetTable = Alphabet.createTable spec
    let spec = Alphabet.translate alphabetTable spec
    let macros =
        let dict = Dictionary()
        for (k, v) in spec.Macros do
            dict.Add(k, v)
        dict
    let rules, nodes =
        List.foldBack
            (fun (name, args, clauses) (perRuleData,dfaNodes) -> 
                let nfa, actions, nfaNodeMap = NfaOfRule macros clauses
                let ruleStartNode, ruleNodes = NfaToDfa nfaNodeMap nfa
                (ruleStartNode,actions) :: perRuleData, ruleNodes @ dfaNodes)
            spec.Rules
            ([],[])
    (alphabetTable, rules, nodes)
