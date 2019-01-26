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
let mutable parslib = "FsLexYaccLite.Parsing"

let usage =
    [("-o", StringArg (fun s -> output <- Some s), "Name the output file.")
     ("-v", UnitArg (fun () -> verbose <- true), "Produce a listing file.")
     ("--module", StringArg (fun s -> modname <- Some s), "Define the F# module name to host the generated parser.")
     ("--parslib", StringArg (fun s ->  parslib <- s), "Specify the namespace for the implementation of the parser table interpreter (default: Microsoft.FSharp.Text.Parsing)")]

let main() =

    parseCommandLineArgs usage (fun x -> match input with Some _ -> failwith "more than one input given" | None -> input <- Some x) "fsyacc <filename>"

    let input =
        match input with
        | Some x -> x
        | None -> failwith "no input given"

    let output =
        match output with
        | Some x -> x
        | None -> Path.ChangeExtension(input, ".fs")

    let modname =
        match modname with 
        | None ->
            let s = Path.GetFileNameWithoutExtension(input)
            String(Char.ToUpperInvariant s.[0], 1) + s.Substring(1)
        | Some name -> name

    let spec =
        let lexbuf =
            use f = new FileStream(input, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
            use r = new StreamReader(f, true)
            LexBuffer.FromString(r.ReadToEnd())
      
        lexbuf.EndPos <- Position_Zero

        try 
            Parser.spec Lexer.token lexbuf 
        with e -> 
            eprintf "%s(%d,%d): error: %s" input lexbuf.StartPos.Line lexbuf.StartPos.Column e.Message;
            exit 1
    
    printfn "building tables"; 
    let preprocessed = processParserSpecAst spec
    let compiled = compile preprocessed

    printfn "%d states" compiled.States.Length; 
    printfn "%d nonterminals" compiled.GotoTable.[0].Length; 
    printfn "%d terminals" preprocessed.Terminals.Length
    printfn "%d productions" compiled.Productions.Length; 
    printfn "#rows in action table: %d" compiled.ActionTable.Length

    let actionElements =
        let accu = ResizeArray()
        for i = 0 to compiled.Kernels.Length - 1 do
            for j = 0 to preprocessed.Terminals.Length - 1 do
                let row = compiled.ActionTable.[i]
                match row.LookaheadActions.[j] with
                | Some action ->
                    let k = preprocessed.Terminals.Length * i + j
                    let v =
                        match action with
                        | Shift x -> x
                        | Reduce y -> ~~~ y
                        | Accept -> int Int16.MaxValue
                        | Error -> int Int16.MinValue
                    accu.Add((k, v))
                | None -> ()
        accu.ToArray()

    let actionHashtable = Hashtable.create None actionElements

    let gotoElements =
        let accu = ResizeArray()
        for i = 0 to compiled.Kernels.Length - 1 do
            for j = 0 to preprocessed.NonTerminals.Length - 1 do
                match compiled.GotoTable.[i].[j] with
                | Some v ->
                    let k = preprocessed.NonTerminals.Length * i + j
                    accu.Add(k, v)
                | None -> ()
        accu.ToArray()
    
    let gotoHashtable = Hashtable.create None gotoElements

    YaccOutput.outputParser output modname parslib (spec.Header) spec preprocessed compiled actionHashtable gotoHashtable

    if verbose then
        YaccOutput.outputCompilationReport (input + ".html") preprocessed compiled
        YaccOutput.outputTableImages input preprocessed compiled

        Hashtable.outputHashtableStat compiled.Kernels.Length preprocessed.Terminals.Length actionHashtable
        Hashtable.outputHashtableImage (input + "-actionHashtableImage.png") actionHashtable

        Hashtable.outputHashtableStat compiled.Kernels.Length preprocessed.NonTerminals.Length gotoHashtable
        Hashtable.outputHashtableImage (input + "-gotoHashtableImage.png") gotoHashtable

#if DEBUG
main()
#else
try
    main()
with e -> 
    Console.Error.WriteLine(e.Message)
    exit 1
#endif