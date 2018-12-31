module FsLexYacc.FsYacc.Preprocess

open Syntax

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
