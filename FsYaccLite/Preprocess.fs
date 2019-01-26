module Preprocess

open System.Collections.Generic
open Syntax

type Production =
    { Head : string
      Body : string array
      PrecedenceInfo : (Associativity * int * string) option
      Code : Code }

type Preprocessed = 
    { Tokens : (string * string option) []
      Types : Dictionary<string, string>
      Terminals: (string * (Associativity * int) option) []
      NonTerminals: string []
      Symbols : string []
      Productions: Production []
      OriginalStartSymbols : string []
      StartSymbols: string [] }

let endOfInputTerminal = "$"
let dummyLookahead = "#"
let errorTerminal = "error"

let processParserSpecAst (spec : ParserSpec) =
    let tokens = ResizeArray()
    let types = Dictionary(HashIdentity.Structural)
    let startSymbols = ResizeArray()
    let assocs = ResizeArray()

    for decl in spec.Decls do
        match decl with
        | Token (ty, names) ->
            for name in names do
                tokens.Add((name, ty))
        | Type (ty, idents) ->
            for ident in idents do
                try
                    types.Add(ident, ty)
                with _ ->
                    failwithf "%s is given multiple %%type declarations" ident
        | Start idents -> startSymbols.AddRange(idents)
        | Prec (assoc, idents) -> assocs.Add((assoc, idents))
    
    let explicitPrecInfo = 
        assocs
        |> List.ofSeq
        |> List.mapi (fun i (assoc, idents) -> List.map (fun precSym -> (precSym, (assoc, i))) idents)
        |> List.concat
    
    for key, _ in explicitPrecInfo |> Seq.countBy fst |> Seq.filter (fun (_, n) -> n > 1)  do
        failwithf "%s is given two associativities" key
    
    let explicitPrecInfo = Map.ofList explicitPrecInfo
    let terminals = Array.append (Array.map fst (tokens.ToArray())) [| errorTerminal |]
    let terminalsSet = HashSet(terminals)
       
    let productions =  
        spec.Rules |> List.mapi (fun i (nonterm,rules) -> 
            rules |> List.mapi (fun j rule -> 
                let precInfo = 
                    let precsym = List.foldBack (fun x acc -> match acc with Some _ -> acc | None -> match x with z when terminalsSet.Contains z -> Some z | _ -> acc) rule.Symbols rule.PrecSymbol
                    match precsym with 
                    | Some sym ->
                        if explicitPrecInfo.ContainsKey(sym) then
                            let x, y = explicitPrecInfo.[sym]
                            Some (x, y, sym)
                        else None
                    | None -> None
                { Head = nonterm; PrecedenceInfo = precInfo; Body = Array.ofList rule.Symbols; Code = rule.Code }))
         |> List.concat
         |> Array.ofList

    let nonTerminals = Array.map fst (Array.ofList spec.Rules)
    let nonTerminalSet = HashSet(nonTerminals)

    let checkNonTerminal nt =  
        if nt <> errorTerminal && not (nonTerminalSet.Contains(nt)) then 
            failwith (sprintf "NonTerminal '%s' has no productions" nt)

    for prod in productions do
        for sym in prod.Body do
            if nonTerminalSet.Contains(sym) then
                checkNonTerminal sym 
            else
                if not (terminalsSet.Contains sym) then failwith (sprintf "token %s is not declared" sym)
           
    if startSymbols.Count = 0 then (failwith "at least one %start declaration is required\n")

    for id in startSymbols do
          if not (types.ContainsKey id) then 
            failwith ("a %type declaration is required for start token " + id)
    
    for kv in types do 
        checkNonTerminal kv.Key

    let terminals = Array.map (fun t -> (t, explicitPrecInfo.TryFind t)) terminals
    let startSymbols = startSymbols.ToArray()
    
    // Augment the grammar 
    let fakeStartSymbols = Array.map (fun nt -> "_start" + nt) startSymbols
    let nonTerminals = Array.append fakeStartSymbols nonTerminals
    let terminals = Array.append terminals [| (dummyLookahead, None); (endOfInputTerminal, None) |]
    let productions =
        Array.append
            (Array.map2
                (fun fakeStartNonTerminal startSymbol ->
                    { Head = fakeStartNonTerminal; PrecedenceInfo = None; Body = [| startSymbol |]; Code = null })
                fakeStartSymbols
                startSymbols)
            productions

    { Tokens = tokens.ToArray()
      Types = types
      Terminals = terminals
      NonTerminals = nonTerminals
      Symbols = Array.append (Array.map fst terminals) nonTerminals
      Productions = productions
      OriginalStartSymbols = startSymbols
      StartSymbols = fakeStartSymbols }
