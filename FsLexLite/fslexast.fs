(* (c) Microsoft Corporation 2005-2008.  *)

module FsLexYaccLite.Lex.AST

open System
open System.Collections.Generic

open FsLexYaccLite.Lex.Syntax

type MultiMap<'a, 'b> = Dictionary<'a, 'b list>

let createMultiMap() : MultiMap<_, _> = Dictionary()

let AddToMultiMap (trDict:MultiMap<_,_>) a b =
    let prev = if trDict.ContainsKey(a) then trDict.[a] else []
    trDict.[a] <- b::prev

type NfaNode = 
    { Id : int
      Transitions: Dictionary<Alphabet, NfaNode list>
      Accepted: (int * int) option }

type NfaNodeMap = List<NfaNode>

let Epsilon = -1

let LexerStateToNfa (macros: Map<string, Regexp>) (clauses: Clause list) = 

    /// Table allocating node ids 
    let nfaNodeMap = new NfaNodeMap()
    
    let newNfaNode trs ac =
        let nodeId = nfaNodeMap.Count
        let trDict = createMultiMap()
        for (a, b) in trs do
            AddToMultiMap trDict a b
        let node = { Id = nodeId; Transitions = trDict; Accepted=ac }
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
            if not (macros.ContainsKey(m)) then failwith ("The macro "+m+" is not defined");
            CompileRegexp (macros.[m]) dest 
        | _ -> failwith "dontcare"

    let actions = new System.Collections.Generic.List<_>()
    
    /// Compile an acceptance of a regular expression into the NFA
    let sTrans macros nodeId (regexp,code) = 
        let actionId = actions.Count
        actions.Add(code)
        let sAccept = newNfaNode [] (Some (nodeId,actionId))
        CompileRegexp regexp sAccept 

    let trs = clauses |> List.mapi (fun n x -> (Epsilon,sTrans macros n x)) 
    let nfaStartNode = newNfaNode trs None
    nfaStartNode,(actions |> Seq.readonly), nfaNodeMap

type NfaNodeIdSetBuilder = HashSet<int>
let createNfaNodeIdSetBuilder() = HashSet<int>()

type NfaNodeIdSet = int array
let createNfaNodeIdSet (builder : NfaNodeIdSetBuilder) =
    let ary = Array.zeroCreate<int> builder.Count
    builder.CopyTo(ary)
    Array.sortInPlace ary
    ary

let newDfaNodeId = 
    let i = ref 0 
    fun () -> let res = !i in incr i; res

type DfaNode = 
    { Id: int
      mutable Transitions : (Alphabet * DfaNode) list
      Accepted: (int * int) array }

let NfaToDfa (nfaNodeMap : NfaNodeMap) nfaStartNode = 

    let rec EClosure1 (acc:NfaNodeIdSetBuilder) (n:NfaNode) = 
        if not (acc.Contains(n.Id)) then 
            acc.Add(n.Id) |> ignore;
            if n.Transitions.ContainsKey(Epsilon) then
                match n.Transitions.[Epsilon] with 
                | [] -> () // this Clause is an optimization - the list is normally empty
                | tr -> 
                    //printfn "n.Id = %A, #Epsilon = %d" n.Id tr.Length
                    tr |> List.iter (EClosure1 acc) 

    let EClosure (moves : list<int>) = 
        let acc = createNfaNodeIdSetBuilder()
        for i in moves do
            EClosure1 acc nfaNodeMap.[i]
        createNfaNodeIdSet acc

    // Compute all the immediate one-step moves for a set of NFA states, as a dictionary
    // mapping inputs to destination lists
    let ComputeMoves (nset:NfaNodeIdSet) = 
        let moves = createMultiMap()
        Array.iter (fun nodeId -> 
            for (KeyValue(inp,dests)) in nfaNodeMap.[nodeId].Transitions do
                if inp <> Epsilon then 
                    match dests with 
                    | [] -> ()  // this Clause is an optimization - the list is normally empty
                    | tr -> tr |> List.iter(fun dest -> AddToMultiMap moves inp dest.Id)) nset
        moves

    let acc = createNfaNodeIdSetBuilder()
    EClosure1 acc nfaStartNode;
    let nfaSet0 = createNfaNodeIdSet acc

    let dfaNodes = ref (Map.empty<NfaNodeIdSet,DfaNode>)

    let GetDfaNode nfaSet = 
        if (!dfaNodes).ContainsKey(nfaSet) then 
            (!dfaNodes).[nfaSet]
        else 
            let dfaNode =
                { Id = newDfaNodeId()
                  Transitions = []
                  Accepted =
                    let accu = List()
                    for nid in nfaSet do
                        let nfaNode = nfaNodeMap.[nid]
                        match nfaNode.Accepted with
                        | Some acc -> accu.Add(acc)
                        | None -> ()
                    accu.ToArray() }

            dfaNodes := (!dfaNodes).Add(nfaSet,dfaNode); 
            dfaNode
            
    let workList = ref [nfaSet0]
    let doneSet = ref Set.empty

    //let count = ref 0 
    let rec Loop () = 
        match !workList with 
        | [] -> ()
        | nfaSet ::t -> 
            workList := t;
            if (!doneSet).Contains(nfaSet) then 
                Loop () 
            else
                let moves = ComputeMoves nfaSet
                for (KeyValue(inp,movesForInput)) in moves do
                    assert (inp <> Epsilon)
                    let moveSet = EClosure movesForInput
                    if moveSet.Length <> 0 then 
                        //incr count
                        let dfaNode = GetDfaNode nfaSet
                        dfaNode.Transitions <- (inp, GetDfaNode moveSet) :: dfaNode.Transitions;
                        (* Printf.printf "%d (%s) : %s --> %d (%s)\n" dfaNode.Id dfaNode.Name (match inp with EncodeChar c -> String.make 1 c | LEof -> "eof") moveSetDfaNode.Id moveSetDfaNode.Name;*)
                        workList := moveSet :: !workList;

                doneSet := (!doneSet).Add(nfaSet);


                Loop()
    Loop();
    //Printf.printfn "count = %d" !count;
    let ruleStartNode = GetDfaNode nfaSet0
    let ruleNodes = 
        (!dfaNodes) 
        |> Seq.map (fun kvp -> kvp.Value) 
        |> Seq.toList
        |> List.sortBy (fun s -> s.Id)
    ruleStartNode,ruleNodes

let Compile spec =
    let alphabetTable = Alphabet.createTable spec
    let spec = Alphabet.translate alphabetTable spec
    let macros = Map.ofList spec.Macros
    let rules, nodes =
        List.foldBack
            (fun (name,args,clauses) (perRuleData,dfaNodes) -> 
                let nfa, actions, nfaNodeMap = LexerStateToNfa macros clauses
                let ruleStartNode, ruleNodes = NfaToDfa nfaNodeMap nfa
                //Printf.printfn "name = %s, ruleStartNode = %O" name ruleStartNode.Id;
                (ruleStartNode,actions) :: perRuleData, ruleNodes @ dfaNodes)
            spec.Rules
            ([],[])
    (alphabetTable, rules, nodes)
