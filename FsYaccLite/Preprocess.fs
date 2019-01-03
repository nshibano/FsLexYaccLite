module FsLexYacc.FsYacc.Preprocess

open System.Collections.Generic

open Syntax

type Production =
    { Head : string
      Body : string array
      PrecedenceInfo : (Associativity * int * string) option
      Code : Code option }

type Preprocessed = 
    { Terminals: (string * (Associativity * int) option) []
      NonTerminals: string array
      Symbols : string []
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
    let terminalsSet = HashSet(terminals)
       
    let productions =  
        spec.Rules |> List.mapi (fun i (nonterm,rules) -> 
            rules |> List.mapi (fun j (Rule(syms,precsym,code)) -> 
                let precInfo = 
                    let precsym = List.foldBack (fun x acc -> match acc with Some _ -> acc | None -> match x with z when terminalsSet.Contains z -> Some z | _ -> acc) syms precsym
                    match precsym with 
                    | Some sym ->
                        if explicitPrecInfo.ContainsKey(sym) then
                            let x, y = explicitPrecInfo.[sym]
                            Some (x, y, sym)
                        else None
                    | None -> None
                { Head = nonterm; PrecedenceInfo = precInfo; Body = Array.ofList syms; Code = code }))
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
                    { Head = fakeStartNonTerminal; PrecedenceInfo = None; Body = [| startSymbol |]; Code = None })
                fakeStartSymbols
                startSymbols)
            productions

    { Terminals = terminals
      NonTerminals = nonTerminals
      Symbols = Array.append (Array.map fst terminals) nonTerminals
      Productions = productions
      StartSymbols = fakeStartSymbols }
