open System
open System.Collections.Generic
open System.IO
open Printf
open FsLexYaccLite.Common.Arg
open FsLexYaccLiteRuntime
open Compile

let mutable input = None
let mutable out = None
let mutable lexlib = "FsLexYaccLiteRuntime"
let mutable modname = None

let usage =
    [("-o", StringArg (fun s -> out <- Some s), "Name the output file.")
     ("--module", StringArg (fun s -> modname <- Some s), "Define the F# module name to host the generated parser.")
     ("--lexlib", StringArg (fun s ->  lexlib <- s), "Specify the namespace for the implementation of the lexer table interpreter (default Microsoft.FSharp.Text.Lexing)")]

let [<Literal>] Sentinel = -1

try
    parseCommandLineArgs usage (fun x -> match input with Some _ -> failwith "more than one input given" | None -> input <- Some x) "fslex <filename>"
    let filename = (match input with Some x -> x | None -> failwith "no input given") 
    let spec = 
      let lexbuf =
            use f = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
            use r = new StreamReader(f, true)
            LexBuffer.FromString(r.ReadToEnd())
      lexbuf.EndPos <- Position_Zero
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
        let dfaNodes = compile translated
        printfn "%d states" dfaNodes.Length
        let actions = List.map snd clauses
        (name, args, alphabetTable, dfaNodes, actions)) spec.Rules
    
    printfn "writing output"
    
    let output = 
        match out with 
        | Some x -> x 
        | _ -> Path.Combine (Path.GetDirectoryName filename,Path.GetFileNameWithoutExtension(filename)) + ".fs"

    use os = System.IO.File.CreateText output

    fprintfn os "// GENERATED FILE"
    let moduleName =
        match modname with
        | None ->
            let s = Path.GetFileNameWithoutExtension(filename)
            String(Char.ToUpperInvariant s.[0], 1) + s.Substring(1)
        | Some name -> name
    
    fprintfn os "module %s" moduleName
    
    let printLinesIfCodeDefined (code,pos:Position) =
        if pos <> Position_Empty  // If bottom code is unspecified, then position is empty.        
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
                    else Sentinel
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
                    int16 Sentinel) dfaNodes)

        fprintfn os "let private %s_tables = %s.LexTables(%s_asciiAlphabetTable, %s_nonAsciiCharRangeTable, %s_nonAsciiAlphabetTable, %s_transitionTable, %s_acceptTable)" name lexlib name name name name name
    
    for i = 0 to rules.Length - 1 do
        let name, args, alphabetTable, dfaNodes, actions = rules.[i]
        fprintfn os "%s %s%s lexbuf =" (if i = 0 then "let rec" else "and") name (String.concat "" (List.map (fun s -> " " + s) args))
        fprintfn os "    match %s_tables.Interpret(lexbuf) with" name
        Seq.iteri (fun i (code : string, _) ->
            if (Output.getLines code).Length > 1 then
                fprintfn os "    | %d ->" i
                Output.outputCode os 8 code
            else
                fprintfn os "    | %d -> %s" i (code.Trim(' '))) actions

        fprintfn os "    | _ -> failwith \"%s\"" name

    fprintfn os ""
        
    printLinesIfCodeDefined spec.BottomCode
    
with e ->
    match e with
    | Failure s ->
        eprintfn "%s" s;
        exit 1
    | _ -> raise e