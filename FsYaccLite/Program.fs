open System
open System.IO 
open System.Collections.Generic
open Printf
open FsLexYaccLite.Lexing
open FsLexYaccLite.Common.Arg
open Syntax
open Preprocess
open Compile

let mutable input = None
let mutable modname= None
let mutable output = None
let mutable verbose = false
let mutable lexlib = "FsLexYaccLite.Lexing"
let mutable parslib = "FsLexYaccLite.Parsing"

let usage =
    [("-o", StringArg (fun s -> output <- Some s), "Name the output file.")
     ("-v", UnitArg (fun () -> verbose <- true), "Produce a listing file.")
     ("--module", StringArg (fun s -> modname <- Some s), "Define the F# module name to host the generated parser.")
     ("--lexlib", StringArg (fun s ->  lexlib <- s), "Specify the namespace for the implementation of the lexer (default: Microsoft.FSharp.Text.Lexing)")
     ("--parslib", StringArg (fun s ->  parslib <- s), "Specify the namespace for the implementation of the parser table interpreter (default: Microsoft.FSharp.Text.Parsing)")]

let main() =
    parseCommandLineArgs usage (fun x -> match input with Some _ -> failwith "more than one input given" | None -> input <- Some x) "fsyacc <filename>"

    let input =
        match input with
        | Some x -> x
        | None -> failwith "no input given"

    let spec =
        let lexbuf =
            use f = new FileStream(input, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
            use r = new StreamReader(f, true)
            LexBuffer.FromString(r.ReadToEnd())
      
        lexbuf.EndPos <- Position.FirstLine(input)

        try 
        Parser.spec Lexer.token lexbuf 
        with e -> 
            eprintf "%s(%d,%d): error: %s" input lexbuf.StartPos.Line lexbuf.StartPos.Column e.Message;
            exit 1  in
  
    let spec = { spec with Tokens = List.rev spec.Tokens }
  
    let output =
        match output with
        | Some x -> x
        | None -> Path.ChangeExtension(input, ".fs")

    printfn "building tables"; 
    let preprocessed = processParserSpecAst spec
    let compiled = compile preprocessed
    let (code, _) = spec.Header

    printfn "%d states" compiled.States.Length; 
    printfn "%d nonterminals" compiled.GotoTable.[0].Length; 
    printfn "%d terminals" preprocessed.Terminals.Length
    printfn "%d productions" compiled.Productions.Length; 
    printfn "#rows in action table: %d" compiled.ActionTable.Length
    YaccOutput.outputParser input output modname lexlib parslib code spec preprocessed compiled

    if verbose then
        use f = (File.CreateText (input + ".html")) :> TextWriter
        fprintfn f "<pre>"
        fprintfn f "Output file describing compiled parser placed in %s" output
        YaccOutput.outputCompilationReport f preprocessed compiled
        fprintfn f "</pre>"

        YaccOutput.outputTableImages input preprocessed compiled

        let actionMatrix =
            Array.map (fun (row : ActionTableRow) ->
                let ary = Array.create preprocessed.Terminals.Length None
                Array.iter (fun (i, _) ->
                    ary.[i] <- Some 0) row.LookaheadActions
                ary) compiled.ActionTable
      
        let actionHashtable = Hashtable.create None actionMatrix
        Hashtable.outputHashtableStat actionMatrix actionHashtable
        Hashtable.outputHashtableImage (input + "-actionHashtableImage.png") actionHashtable

        let gotoHashtable = Hashtable.create None compiled.GotoTable
        Hashtable.outputHashtableStat compiled.GotoTable gotoHashtable
        Hashtable.outputHashtableImage (input + "-gotoHashtableImage.png") gotoHashtable

try
    main()
with e -> 
    Console.Error.WriteLine(e.Message)
    exit 1

