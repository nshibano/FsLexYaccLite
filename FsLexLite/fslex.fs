// (c) Microsoft Corporation 2005-2009.  

module internal FsLexYacc.Lex.Driver 

open FsLexYaccLite.Common.Arg
open FsLexYaccLite.Lex
open FsLexYaccLite.Lex.Syntax
open FsLexYaccLite.Lex.AST
open Printf
//open Internal.Utilities
//open Internal.Utilities.Text.Lexing
open FsLexYaccLite.Lexing
open System
open System.Collections.Generic
open System.IO

//------------------------------------------------------------------
// This code is duplicated from Microsoft.FSharp.Compiler.UnicodeLexing

type Lexbuf =  LexBuffer

    
//------------------------------------------------------------------
// This is the program proper

let input = ref None
let out = ref None
let inputCodePage = ref None
let light = ref None

let mutable lexlib = "FsLexYaccLite.Lexing"

let usage =
  [ ("-o", StringArg (fun s -> out := Some s), "Name the output file."); 
    ("--codepage", IntArg (fun i -> inputCodePage := Some i), "Assume input lexer specification file is encoded with the given codepage."); 
    ("--light", UnitArg (fun () ->  light := Some true), "(ignored)");
    ("--light-off", UnitArg (fun () ->  light := Some false), "Add #light \"off\" to the top of the generated file");
    ("--lexlib", StringArg (fun s ->  lexlib <- s), "Specify the namespace for the implementation of the lexer table interpreter (default Microsoft.FSharp.Text.Lexing)");
    ("--unicode", UnitArg (fun () -> ()), "Ignored.");  
  ]

let _ = parseCommandLineArgs usage (fun x -> match !input with Some _ -> failwith "more than one input given" | None -> input := Some x) "fslex <filename>"

let sentinel = -1

let main() = 
  try 
    let filename = (match !input with Some x -> x | None -> failwith "no input given") 
    let spec = 
      let lexbuf = LexBuffer.FromString(File.ReadAllText(filename))

      try 
          Parser.spec Lexer.token lexbuf 
      with e -> 
          eprintf "%s(%d,%d): error: %s" filename lexbuf.StartPos.Line lexbuf.StartPos.Column 
              (match e with 
               | Failure s -> s 
               | _ -> e.Message);
          exit 1
    
    let rules = List.map (fun (name, args, clauses) ->
        printfn "compiling lexing table for rule \"%s\"" name
        let regexps = List.map fst clauses
        let expanded = Macros.expand spec.Macros regexps
        let alphabetTable = Alphabet.createTable true expanded
        printfn "%d alphabets" alphabetTable.AlphabetCount
        let translated = Alphabet.translate alphabetTable expanded
        let dfaNodes = AST.Compile translated
        printfn "%d states" dfaNodes.Length
        let actions = List.map snd clauses
        (name, args, alphabetTable, dfaNodes, actions)) spec.Rules
    
    printfn "writing output"
    
    let output = 
        match !out with 
        | Some x -> x 
        | _ -> 
            Path.Combine (Path.GetDirectoryName filename,Path.GetFileNameWithoutExtension(filename)) + ".fs"
    use os = System.IO.File.CreateText output
    let moduleName =
        let s = Path.GetFileNameWithoutExtension(filename)
        String(Char.ToUpperInvariant s.[0], 1) + s.Substring(1)
    fprintf os "module %s" moduleName
    if (!light = Some(false)) || (!light = None && (Path.HasExtension(output) && Path.GetExtension(output) = ".ml")) then
        fprintfn os "#light \"off\"";
    
    let printLinesIfCodeDefined (code,pos:Position) =
        if pos <> Position.Empty  // If bottom code is unspecified, then position is empty.        
        then 
            fprintfn os "%s" code;
    
    let printInt16Array (name : string) (ary : int16 array) =
        fprintf os "let private %s = [| " name
        for i = 0 to ary.Length - 1 do
            if i <> 0 then fprintf os "; "
            fprintf os "%ds" ary.[i]
        fprintfn os " |]"
    
    let printUInt16Array (name : string) (ary : uint16 array) =
        fprintf os "let private %s = [| " name
        for i = 0 to ary.Length - 1 do
            if i <> 0 then fprintf os "; "
            fprintf os "%dus" ary.[i]
        fprintfn os " |]"
    
    printLinesIfCodeDefined spec.TopCode

    for name, args, alphabetTable, dfaNodes, actions in rules do

        printUInt16Array (name + "_asciiAlphabetTable") (Array.map uint16 (Array.sub alphabetTable.IndexTable 0 128))
        printUInt16Array (name + "_nonAsciiCharRangeTable") (Array.map uint16 (Array.sub alphabetTable.RangeTable 128 (alphabetTable.RangeTable.Length - 128)))
        printUInt16Array (name + "_nonAsciiAlphabetTable") (Array.map uint16 (Array.sub alphabetTable.IndexTable 128 (alphabetTable.IndexTable.Length - 128)))

        fprintfn os "let private %s_transitionTable =" name
        fprintfn os "    [|";

        for state in dfaNodes do
            fprintf os "        [| "
            let trans = 
                let dict = Dictionary()
                Seq.iter dict.Add state.Transitions
                dict
            let emit n = 
                let code =
                    if trans.ContainsKey(n) then 
                        trans.[n].Id 
                    else sentinel
                fprintf os "%ds" code
            for i = 0 to alphabetTable.AlphabetCount - 1 do 
                emit i
                fprintf os "; "
            emit alphabetTable.AlphabetEof
            fprintfn os " |]"
        fprintfn os "    |]"
    
        printInt16Array (name + "_acceptTable")
            (Array.map (fun state ->
                if state.Accepted.Length > 0 then 
                    int16 state.Accepted.[0]
                else
                    int16 sentinel) dfaNodes)

        fprintfn os "let private %s_tables = %s.UnicodeTables(%s_asciiAlphabetTable, %s_nonAsciiCharRangeTable, %s_nonAsciiAlphabetTable, %s_transitionTable, %s_acceptTable)" name lexlib name name name name name
    
    for i = 0 to rules.Length - 1 do
        let name, args, alphabetTable, dfaNodes, actions = rules.[i]
        fprintfn os "%s %s%s lexbuf =" (if i = 0 then "let rec" else "and") name (String.concat "" (List.map (fun s -> " " + s) args))
        fprintfn os "    match %s_tables.Interpret(lexbuf) with" name
        Seq.iteri (fun i (code : string, _) ->
            fprintfn os "    | %d ->" i;
            let lines = code.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
            let getIndentLevel (s : string) =
                let mutable level = 0
                let mutable pos = 0
                while pos < s.Length do
                    match s.[pos] with
                    | ' ' ->
                        level <- level + 1
                        pos <- pos + 1
                    | '\t' ->
                        level <- level + 4
                        pos <- pos + 1
                    | _ -> pos <- s.Length
                level

            let shiftIndentLevel incr (s : string) =
                let current = getIndentLevel s
                String(' ', current + incr) + s.TrimStart([|' '; '\t'|])

            let setIndent (level : int) (lines : string array) =
                let min = Array.min (Array.map getIndentLevel lines)
                Array.map (shiftIndentLevel (level - min)) lines

            for line in setIndent 8 lines do
                fprintfn os "%s" line) actions
        fprintfn os "    | _ -> failwith \"%s\"" name

    fprintfn os ""
        
    printLinesIfCodeDefined spec.BottomCode
    
  with e -> 
    eprintf "FSLEX: error FSL000: %s" (match e with Failure s -> s | e -> e.ToString());
    exit 1


let _ = main()
