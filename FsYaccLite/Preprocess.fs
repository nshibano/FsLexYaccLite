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
    let tokens = ResizeArray()
    let types = Dictionary()
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
        let accu = Dictionary()
        for i = 0 to assocs.Count - 1 do
            let assoc, idents = assocs.[i]
            for ident in idents do
                if accu.ContainsKey(ident) then failwithf "%s is given more than one associativities" ident
                accu.Add(ident, (assoc ,i))
        accu
    
    let terminals = Array.append (Array.map fst (tokens.ToArray())) [| errorTerminal |]
    let terminalsSet = HashSet(terminals)
       
    let productions =
        let accu = ResizeArray()
        for name, optType, clauses in spec.Rules do
            for clause in clauses do
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
                accu.Add({ Head = name; PrecedenceInfo = precInfo; Body = Array.ofList clause.Symbols; Code = clause.Code })
        accu.ToArray()

    let nonTerminals = Array.map (fun (name, _, _) -> name) (Array.ofList spec.Rules)
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
           
    if startSymbols.Count = 0 then (failwith "at least one %start declaration is required\n")

    for id in startSymbols do
          if not (types.ContainsKey id) then 
            failwith ("a %type declaration is required for start token " + id)
    
    for kv in types do 
        checkNonTerminalHasProduction kv.Key

    let terminals = Array.map (fun t -> (t, match explicitPrecInfo.TryGetValue t with | true, x -> Some x | false, _ -> None)) terminals
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
