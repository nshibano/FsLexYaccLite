module Preprocess

open System.Collections.Generic
open Syntax

type Production =
    { Head : string
      Body : string array
      PrecedenceInfo : (Assoc * int * string) option
      Code : string }

type Preprocessed = 
    { Tokens : (string * string option) []
      Types : Dictionary<string, string>
      Terminals: (string * (Assoc * int) option) []
      NonTerminals: string []
      Symbols : string []
      Productions: Production []
      OriginalStartSymbols : string []
      StartSymbols: string [] }

let endOfInputTerminal = "$"
let dummyLookahead = "#"
let errorTerminal = "error"

let processParserSpecAst (spec : ParserSpec) =

    let tokens, types, startSymbols, assocs =
        let tokens = ResizeArray()
        let types = Dictionary()
        let startSymbols = ResizeArray()
        let assocs = ResizeArray()

        let addType ident ty =
            try
                types.Add(ident, ty)
            with _ ->
                failwithf "%s is given multiple %%type declarations" ident

        for decl in spec.Decls do
            match decl with
            | Token (ty, names) ->
                for name in names do
                    tokens.Add((name, ty))
            | Type (ty, idents) ->
                for ident in idents do
                    addType ident ty
            | Start idents -> startSymbols.AddRange(idents)
            | Prec (assoc, idents) -> assocs.Add((assoc, idents))
        
        for rule in spec.Rules do
            Option.iter (addType rule.Ident) rule.Type

        tokens.ToArray(), types, startSymbols.ToArray(), assocs.ToArray()
    
    let explicitPrecInfo =
        let accu = Dictionary()
        for i = 0 to assocs.Length - 1 do
            let assoc, idents = assocs.[i]
            for ident in idents do
                if accu.ContainsKey(ident) then failwithf "%s is given more than one associativities" ident
                accu.Add(ident, (assoc, i))
        accu
    
    let terminals = Array.append (Array.map fst (tokens)) [| errorTerminal |]
    let terminalsSet = HashSet(terminals)
       
    let productions =
        let accu = ResizeArray()
        for rule in spec.Rules do
            for clause in rule.Clauses do
                let precInfo = 
                    let precSym =
                        List.foldBack
                            (fun symbol accu ->
                                match accu with
                                | Some _ -> accu 
                                | None ->
                                    if terminalsSet.Contains(symbol) then
                                        Some symbol
                                    else accu) clause.Symbols clause.PrecSymbol
                    match precSym with 
                    | Some sym ->
                        if explicitPrecInfo.ContainsKey(sym) then
                            let x, y = explicitPrecInfo.[sym]
                            Some (x, y, sym)
                        else None
                    | None -> None
                accu.Add({ Head = rule.Ident; PrecedenceInfo = precInfo; Body = Array.ofList clause.Symbols; Code = clause.Code })
        accu.ToArray()

    let nonTerminals = Array.map (fun rule -> rule.Ident) (Array.ofList spec.Rules)
    let nonTerminalSet = HashSet(nonTerminals)

    let checkNonTerminalHasProduction nt =  
        if nt <> errorTerminal && not (nonTerminalSet.Contains(nt)) then 
            failwith (sprintf "NonTerminal '%s' has no productions" nt)

    for prod in productions do
        for sym in prod.Body do
            if nonTerminalSet.Contains(sym) then
                checkNonTerminalHasProduction sym 
            else
                if not (terminalsSet.Contains sym) then failwith (sprintf "token %s is not declared" sym)
           
    if startSymbols.Length = 0 then (failwith "at least one %start declaration is required\n")

    for id in startSymbols do
          if not (types.ContainsKey id) then 
            failwith ("a %type declaration is required for start token " + id)
    
    for kv in types do 
        checkNonTerminalHasProduction kv.Key

    let terminals = Array.map (fun t -> (t, match explicitPrecInfo.TryGetValue t with | true, x -> Some x | false, _ -> None)) terminals
    
    // Augment the grammar
    let fakeStartSymbols = Array.map (fun nt -> "_start" + nt) startSymbols
    Array.iter2 (fun fake orig -> match types.TryGetValue orig with true, ty -> types.Add(fake, ty) | false, _ -> ()) fakeStartSymbols startSymbols

    let terminals = Array.append terminals [| (dummyLookahead, None); (endOfInputTerminal, None) |]
    let productions =
        Array.append
            (Array.map2
                (fun fakeStartNonTerminal startSymbol ->
                    { Head = fakeStartNonTerminal; PrecedenceInfo = None; Body = [| startSymbol |]; Code = null })
                fakeStartSymbols
                startSymbols)
            productions

    { Tokens = tokens
      Types = types
      Terminals = terminals
      NonTerminals = Array.append fakeStartSymbols nonTerminals
      Symbols = Array.append (Array.map fst terminals) nonTerminals
      Productions = productions
      OriginalStartSymbols = startSymbols
      StartSymbols = fakeStartSymbols }
