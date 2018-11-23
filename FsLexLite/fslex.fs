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

let lineCount = ref 0
let cfprintfn (os: #TextWriter) fmt = Printf.kfprintf (fun () -> incr lineCount; os.WriteLine()) os fmt

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
        cfprintfn os "#light \"off\"";
    
    let printLinesIfCodeDefined (code,pos:Position) =
        if pos <> Position.Empty  // If bottom code is unspecified, then position is empty.        
        then 
            cfprintfn os "%s" code;

    printLinesIfCodeDefined spec.TopCode
    let code = fst spec.TopCode
    lineCount := !lineCount + code.Replace("\r","").Split([| '\n' |]).Length;
    fprintf os "let alphabetTable : uint16[] = [| "
    for i = 0 to alphabetTable.Count - 1 do
      outputCodedUInt16 os alphabetTable.[i]
    cfprintfn os " |]"




    cfprintfn os "let transitionTable : int16[][] = ";
    cfprintfn os "    [| ";
    //let specificUnicodeChars = GetSpecificUnicodeChars()
    // This emits a (numLowUnicodeChars+NumUnicodeCategories+(2*#specificUnicodeChars)+1) * #states array of encoded UInt16 values
        
    // Each row for the Unicode table has format 
    //      128 entries for ASCII characters
    //      A variable number of 2*UInt16 entries for SpecificUnicodeChars 
    //      30 entries, one for each UnicodeCategory
    //      1 entry for EOF
    //
    // Each entry is an encoded UInt16 value indicating the next state to transition to for this input.
    //
    // For the SpecificUnicodeChars the entries are char/next-state pairs.
    for state in dfaNodes do
        //cfprintfn os "    (* State %d *)" state.Id;
        fprintf os "     [| ";
        let trans = 
            let dict = new Dictionary<_,_>()
            state.Transitions |> List.iter dict.Add
            dict
        let emit n = 
            if trans.ContainsKey(n) then 
                outputCodedInt16 os trans.[n].Id 
            else
                outputCodedInt16 os sentinel
        for i = 0 to Alphabet.alphabetsCount alphabetTable - 1 do 
            let c = char i
            emit (int c);
        //for c in specificUnicodeChars do 
        //    outputCodedUInt16 os (int c); 
        //    emit (EncodeChar c);
        //for i = 0 to NumUnicodeCategories-1 do 
        //    emit (EncodeUnicodeCategoryIndex i);
        emit (Alphabet.alphabetEof alphabetTable);
        cfprintfn os "|];"
    done;
        
    cfprintfn os "    |] ";
    
    fprintf os "let acceptTable : int16[] = [|";
    for state in dfaNodes do
        if state.Accepted.Length > 0 then 
          outputCodedInt16 os (snd state.Accepted.Head)
        else
          outputCodedInt16 os sentinel
    done;
    cfprintfn os "|]";
    cfprintfn os "let scanner = %s.UnicodeTables(alphabetTable, transitionTable, acceptTable)" lexlib
    
    cfprintfn os "let rec _fslex_dummy () = _fslex_dummy() ";

    // These actions push the additional start state and come first, because they are then typically inlined into later
    // rules. This means more tailcalls are taken as direct branches, increasing efficiency and 
    // improving stack usage on platforms that do not take tailcalls.
    for ((startNode, actions),(ident,args,_)) in List.zip perRuleData spec.Rules do
        cfprintfn os "(* Rule %s *)" ident;
        cfprintfn os "and %s %s (lexbuf : %s.LexBuffer) = _fslex_%s %s %d lexbuf" ident (String.Join(" ",Array.ofList args)) lexlib ident (String.Join(" ",Array.ofList args)) startNode.Id;
    for ((startNode, actions),(ident,args,_)) in List.zip perRuleData spec.Rules do
        cfprintfn os "(* Rule %s *)" ident;
        cfprintfn os "and _fslex_%s %s _fslex_state lexbuf =" ident (String.Join(" ",Array.ofList args));
        cfprintfn os "  match scanner.Interpret(_fslex_state,lexbuf) with" ;
        actions |> Seq.iteri (fun i (code,pos) -> 
            cfprintfn os "  | %d -> ( " i;
            let lines = code.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
            for line in lines do
                cfprintfn os "               %s" line;
            cfprintfn os "          )")
        cfprintfn os "  | _ -> failwith \"%s\"" ident
    

    cfprintfn os "";
        
    printLinesIfCodeDefined spec.BottomCode
    
  with e -> 
    eprintf "FSLEX: error FSL000: %s" (match e with Failure s -> s | e -> e.ToString());
    exit 1


let _ = main()
