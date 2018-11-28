// (c) Microsoft Corporation 2005-2009.  

module internal FsLexYacc.Lex.Driver 

open FsLexYaccLite.Common.Arg
open FsLexYaccLite.Lex
open FsLexYaccLite.Lex.Syntax
open FsLexYaccLite.Lex.AST
open Printf
//open Internal.Utilities
//open Internal.Utilities.Text.Lexing
open Microsoft.FSharp.Text
open Microsoft.FSharp.Text.Lexing
open System
open System.Collections.Generic
open System.IO
open FsLexYaccLite.Lex.Alphabet

//------------------------------------------------------------------
// This code is duplicated from Microsoft.FSharp.Compiler.UnicodeLexing

type Lexbuf =  LexBuffer<char>

/// Standard utility to create a Unicode LexBuffer
///
/// One small annoyance is that LexBuffers and not IDisposable. This means 
/// we can't just return the LexBuffer object, since the file it wraps wouldn't
/// get closed when we're finished with the LexBuffer. Hence we return the stream,
/// the reader and the LexBuffer. The caller should dispose the first two when done.
let UnicodeFileAsLexbuf (filename,codePage : int option) : FileStream * StreamReader * Lexbuf =
    // Use the .NET functionality to auto-detect the unicode encoding
    // It also presents the bytes read to the lexer in UTF8 decoded form
    let stream  = new FileStream(filename,FileMode.Open,FileAccess.Read,FileShare.Read) 
    let reader = 
        match codePage with 
        | None -> new  StreamReader(stream,true)
        | Some n -> new  StreamReader(stream,System.Text.Encoding.GetEncoding(n)) 
    let lexbuf = LexBuffer.FromFunction(reader.Read) 
    lexbuf.EndPos <- Position.FirstLine(filename);
    stream, reader, lexbuf
    
//------------------------------------------------------------------
// This is the program proper

let input = ref None
let out = ref None
let inputCodePage = ref None
let light = ref None

let mutable lexlib = "Microsoft.FSharp.Text.Lexing"

let usage =
  [ ("-o", StringArg (fun s -> out := Some s), "Name the output file."); 
    ("--codepage", IntArg (fun i -> inputCodePage := Some i), "Assume input lexer specification file is encoded with the given codepage."); 
    ("--light", UnitArg (fun () ->  light := Some true), "(ignored)");
    ("--light-off", UnitArg (fun () ->  light := Some false), "Add #light \"off\" to the top of the generated file");
    ("--lexlib", StringArg (fun s ->  lexlib <- s), "Specify the namespace for the implementation of the lexer table interpreter (default Microsoft.FSharp.Text.Lexing)");
    ("--unicode", UnitArg (fun () -> ()), "Ignored.");  
  ]

let _ = parseCommandLineArgs usage (fun x -> match !input with Some _ -> failwith "more than one input given" | None -> input := Some x) "fslex <filename>"

let outputInt (os: TextWriter) (n:int) = os.Write(string n)

let outputCodedInt16 (os: #TextWriter)  (n:int) = 
  os.Write n;
  os.Write "s; ";
let outputCodedUInt16 (os: #TextWriter)  (n:int) = 
  os.Write n;
  os.Write "us; ";
let sentinel = -1

let main() = 
  try 
    let filename = (match !input with Some x -> x | None -> failwith "no input given") 
    let spec = 
      let stream,reader,lexbuf = UnicodeFileAsLexbuf(filename, !inputCodePage) 
      use stream = stream
      use reader = reader
      try 
          Parser.spec Lexer.token lexbuf 
      with e -> 
          eprintf "%s(%d,%d): error: %s" filename lexbuf.StartPos.Line lexbuf.StartPos.Column 
              (match e with 
               | Failure s -> s 
               | _ -> e.Message);
          exit 1
    printfn "compiling to dfas (can take a while...)";
    let alphabetTable, perRuleData, dfaNodes = AST.Compile spec
    let dfaNodes = dfaNodes |> List.sortBy (fun n -> n.Id) 

    printfn "%d states" dfaNodes.Length;
    printfn "writing output"; 
    
    let output = 
        match !out with 
        | Some x -> x 
        | _ -> 
            Path.Combine (Path.GetDirectoryName filename,Path.GetFileNameWithoutExtension(filename)) + ".fs"
    use os = System.IO.File.CreateText output

    if (!light = Some(false)) || (!light = None && (Path.HasExtension(output) && Path.GetExtension(output) = ".ml")) then
        fprintfn os "#light \"off\"";
    
    let printLinesIfCodeDefined (code,pos:Position) =
        if pos <> Position.Empty  // If bottom code is unspecified, then position is empty.        
        then 
            fprintfn os "%s" code;
    
    let printInt16Array (name : string) (ary : int16 array) =
        fprintf os "let %s = [| " name
        for i = 0 to ary.Length - 1 do
            if i <> 0 then fprintf os "; "
            fprintf os "%ds" ary.[i]
        fprintfn os " |]"
    
    let printUInt16Array (name : string) (ary : uint16 array) =
        fprintf os "let %s = [| " name
        for i = 0 to ary.Length - 1 do
            if i <> 0 then fprintf os "; "
            fprintf os "%dus" ary.[i]
        fprintfn os " |]"
    
    printLinesIfCodeDefined spec.TopCode

    printUInt16Array "charRangeTable" (Array.map uint16 alphabetTable.RangeTable)
    printUInt16Array "alphabetTable" (Array.map uint16 alphabetTable.IndexTable)

    fprintfn os "let transitionTable =";
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
    
    printInt16Array "acceptTable"
        (Array.map (fun state ->
            if state.Accepted.Length > 0 then 
                int16 (snd state.Accepted.[0])
            else
                int16 sentinel) (Array.ofList dfaNodes))

    fprintfn os "let scanner = %s.UnicodeTables(charRangeTable, alphabetTable, transitionTable, acceptTable)" lexlib
    
    fprintfn os "let rec _fslex_dummy () = _fslex_dummy() "

    // These actions push the additional start state and come first, because they are then typically inlined into later
    // rules. This means more tailcalls are taken as direct branches, increasing efficiency and 
    // improving stack usage on platforms that do not take tailcalls.
    for ((startNode, actions),(ident,args,_)) in List.zip perRuleData spec.Rules do
        fprintfn os "(* Rule %s *)" ident;
        fprintfn os "and %s %s (lexbuf : %s.LexBuffer) = _fslex_%s %s %d lexbuf" ident (String.Join(" ",Array.ofList args)) lexlib ident (String.Join(" ",Array.ofList args)) startNode.Id;
    for ((startNode, actions),(ident,args,_)) in List.zip perRuleData spec.Rules do
        fprintfn os "(* Rule %s *)" ident;
        fprintfn os "and _fslex_%s %s _fslex_state lexbuf =" ident (String.Join(" ",Array.ofList args));
        fprintfn os "  match scanner.Interpret(_fslex_state,lexbuf) with" ;
        actions |> Seq.iteri (fun i (code,pos) -> 
            fprintfn os "  | %d -> ( " i;
            let lines = code.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
            for line in lines do
                fprintfn os "               %s" line;
            fprintfn os "          )")
        fprintfn os "  | _ -> failwith \"%s\"" ident
    

    fprintfn os "";
        
    printLinesIfCodeDefined spec.BottomCode
    
  with e -> 
    eprintf "FSLEX: error FSL000: %s" (match e with Failure s -> s | e -> e.ToString());
    exit 1


let _ = main()
