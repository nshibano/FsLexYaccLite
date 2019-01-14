module Compile

open System.Collections.Generic
open Syntax

type MultiMap<'a, 'b> = Dictionary<'a, List<'b>>

let createMultiMap() : MultiMap<_, _> = Dictionary()

let addToMultiMap (dict : MultiMap<_,_>) a b =
    match dict.TryGetValue(a) with
    | true, l -> l.Add(b)
    | false, _ ->
        let l = List()
        l.Add(b)
        dict.[a] <- l

type NfaNode = 
    { Id : int
      Transitions : MultiMap<Alphabet, NfaNode>
      Accepted: int option }

type NfaNodeMap = List<NfaNode>

let [<Literal>] Epsilon = -1

let nfaOfRule (regexps: Regexp list) = 

    /// Table allocating node ids 
    let nfaNodeMap = new NfaNodeMap()
    
    let newNfaNode trs ac =
        let nodeId = nfaNodeMap.Count
        let trDict = createMultiMap()
        for (a, b) in trs do
            addToMultiMap trDict a b
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
            addToMultiMap nfaNode.Transitions Epsilon sre
            newNfaNode [(Epsilon,sre); (Epsilon,dest)] None
        | _ -> failwith "dontcare"

    let trs = List.mapi (fun nodeId regexp ->
        (Epsilon,
         let sAccept = newNfaNode [] (Some nodeId)
         CompileRegexp regexp sAccept)) regexps
    let nfaStartNode = newNfaNode trs None

    nfaStartNode, nfaNodeMap

type NfaNodeIdSetBuilder = HashSet<int>
let createNfaNodeIdSetBuilder() : NfaNodeIdSetBuilder = HashSet<int>()

type NfaNodeIdSet = int array
let createNfaNodeIdSet (builder : NfaNodeIdSetBuilder) : NfaNodeIdSet =
    let ary = Array.zeroCreate<int> builder.Count
    builder.CopyTo(ary)
    Array.sortInPlace ary
    ary

let dfaNodeIdTop = ref 0
let newDfaNodeId() = 
    let id = !dfaNodeIdTop
    incr dfaNodeIdTop
    id

type DfaNode = 
    { Id: int
      Transitions : List<Alphabet * DfaNode>
      Accepted: int array }

let nfaToDfa (nfaNodeMap : NfaNodeMap) nfaStartNode = 
    dfaNodeIdTop := 0

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
                        addToMultiMap moves inp dest.Id
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
                  Accepted = Array.choose (fun nid -> nfaNodeMap.[nid].Accepted) (Array.sort (Array.ofSeq nfaSet)) }
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

    let dfaNodes = Array.ofSeq dfaNodes.Values
    Array.sortInPlaceBy (fun (node : DfaNode) -> node.Id) dfaNodes
    dfaNodes

let compile (regexps : Regexp list) =
    let nfaStart, nfaNodes = nfaOfRule regexps
    nfaToDfa nfaNodes nfaStart
