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
let mutable out = None
let mutable log = false
let mutable lexlib = "FsLexYaccLite.Parsing"
let mutable parslib = "FsLexYaccLite.Parsing"

let usage =
  [ ("-o", StringArg (fun s -> out <- Some s), "Name the output file.");
    ("-v", UnitArg (fun () -> log <- true), "Produce a listing file."); 
    ("--module", StringArg (fun s -> modname <- Some s), "Define the F# module name to host the generated parser."); 
    ("--lexlib", StringArg (fun s ->  lexlib <- s), "Specify the namespace for the implementation of the lexer (default: Microsoft.FSharp.Text.Lexing)");
    ("--parslib", StringArg (fun s ->  parslib <- s), "Specify the namespace for the implementation of the parser table interpreter (default: Microsoft.FSharp.Text.Parsing)")]

let output_int (os : TextWriter) (n:int) = os.Write(string n)

let outputCodedUInt16 (os: TextWriter) (n:int) = 
  os.Write n;
  os.Write "us; "

let outputUInt16Array (os : TextWriter) (name : string) (ary : int array) =
    fprintf os "let %s = [|" name
    for i = 0 to ary.Length - 1 do  
        if i <> 0 then
            fprintf os "; "
        fprintf os "%dus" ary.[i]
    fprintfn os "|]"

let shiftFlag = 0x0000
let reduceFlag = 0x4000
let errorFlag = 0x8000
let acceptFlag = 0xc000
let actionMask = 0xc000

let anyMarker = 0xffff

let actionCoding action =
  match action with 
  | Accept -> acceptFlag
  | Shift n -> shiftFlag ||| n
  | Reduce n -> reduceFlag ||| n
  | Error -> errorFlag 

let main() =
  let _ = parseCommandLineArgs usage (fun x -> match input with Some _ -> failwith "more than one input given" | None -> input <- Some x) "fsyacc <filename>"

  let filename = (match input with Some x -> x | None -> failwith "no input given") in 

  let spec = 
      let lexbuf = LexBuffer.FromString(File.ReadAllText(filename))
      lexbuf.EndPos <- Position.FirstLine(filename)

      try 
        Parser.spec Lexer.token lexbuf 
      with e -> 
         eprintf "%s(%d,%d): error: %s" filename lexbuf.StartPos.Line lexbuf.StartPos.Column e.Message;
         exit 1  in
  
  let spec = { spec with Tokens = List.rev spec.Tokens }

  let has_extension (s:string) = 
    (s.Length >= 1 && s.[s.Length - 1] = '.') 
    || Path.HasExtension(s)

  let chop_extension (s:string) =
    if not (has_extension s) then invalidArg "s" "the file name does not have an extension"
    Path.Combine (Path.GetDirectoryName s,Path.GetFileNameWithoutExtension(s)) 
  
  let checkSuffix (x:string) (y:string) = x.EndsWith(y)

  let output = match out with Some x -> x | _ -> chop_extension filename + (if checkSuffix filename ".mly" then ".ml" else ".fs") in
  let outputi = match out with Some x -> chop_extension x + (if checkSuffix x ".ml" then ".mli" else ".fsi") | _ -> chop_extension filename + (if checkSuffix filename ".mly" then ".mli" else ".fsi") in
  let outputo = 
      if log then Some (match out with Some x -> chop_extension x + ".fsyacc.html" | _ -> chop_extension filename + ".fsyacc.html") 
      else None 

  use os = (File.CreateText output :> TextWriter)

  let logf = Option.map (fun path -> File.CreateText path :> TextWriter) outputo
  Option.iter (fun f ->
    fprintfn f "<pre>"
    fprintfn f "Output file describing compiled parser placed in %s and %s" output outputi) logf

  printfn "building tables"; 
  let preprocessed = processParserSpecAst spec
  let compiled = compile preprocessed
  Option.iter (fun f -> Print.outputCompilationReport f preprocessed compiled) logf
  if log then
    Print.outputTableImages filename preprocessed compiled 
  Option.iter (fun f -> fprintfn f "</pre>") logf

  let (code,pos) = spec.Header 
  printfn "%d states" compiled.States.Length; 
  printfn "%d nonterminals" compiled.GotoTable.[0].Length; 
  printfn "%d terminals" preprocessed.Terminals.Length
  printfn "%d productions" compiled.Productions.Length; 
  printfn "#rows in action table: %d" compiled.ActionTable.Length; 


  fprintfn os "// Implementation file for parser generated by fsyacc";

  match modname with 
  | None ->
        let moduleName =
            let s = Path.GetFileNameWithoutExtension(filename)
            String(Char.ToUpperInvariant s.[0], 1) + s.Substring(1)
        fprintfn os "module %s" moduleName
  | Some s -> 
          fprintfn os "module %s" s;  
  fprintfn os "#nowarn \"64\""
  fprintfn os "open %s" lexlib;
  fprintfn os "%s" code;
  fprintfn os "type token = ";
  for id,typ in spec.Tokens do 
    match typ with
    | None -> fprintfn os "  | %s" id
    | Some ty -> fprintfn os "  | %s of %s" id ty; 

  fprintfn os "";
  fprintfn os "let tagOfToken (t : token) = ";
  fprintfn os "  match t with";
  spec.Tokens |> List.iteri (fun i (id,typ) -> 
      fprintfn os "  | %s %s -> %d " id (match typ with Some _ -> "_" | None -> "") i);
  fprintfn os "";
  fprintfn os "let endOfInputTag = %d " compiled.EndOfInputTerminalIndex;
  fprintfn os "";
  fprintfn os "let dataOfToken (t : token) : obj = ";
  fprintfn os "  match t with ";

  for (id,typ) in spec.Tokens do
      fprintfn os "  | %s %s -> %s " 
        id
        (match typ with Some _ -> "x" | None -> "")
        (match typ with Some _ -> "box x" | None -> "null")

  for (key,_) in spec.Types |> Seq.countBy fst |> Seq.filter (fun (_,n) -> n > 1)  do
        failwithf "%s is given multiple %%type declarations" key;
    
  for (key,_) in spec.Tokens |> Seq.countBy fst |> Seq.filter (fun (_,n) -> n > 1)  do
        failwithf "%s is given %%token declarations" key
  fprintfn os ""
  
  let types = Map.ofList spec.Types 
  let tokens = Map.ofList spec.Tokens 
  let nStates = compiled.States.Length
  begin 
      fprintf os "let gotos = [| " ;
      let numGotoNonTerminals = compiled.GotoTable.[0].Length 
      let gotoIndexes = Array.create numGotoNonTerminals 0 
      let gotoTableCurrIndex = ref 0 in 
      for j = 0 to numGotoNonTerminals-1 do  
          gotoIndexes.[j] <- !gotoTableCurrIndex;

          (* Count the number of entries in the association table. *)
          let count = ref 0 in 
          for i = 0 to nStates - 1 do 
            let goto = compiled.GotoTable.[i].[j] 
            match goto with 
            | None -> ()
            | Some _ -> incr count
   
          (* Write the head of the table (i.e. the number of entries and the default value) *)
          gotoTableCurrIndex := !gotoTableCurrIndex + 1;
          outputCodedUInt16 os !count;
          outputCodedUInt16 os anyMarker;
          
          (* Write the pairs of entries in incremental order by key *)
          (* This lets us implement the lookup by a binary chop. *)
          for i = 0 to nStates - 1 do 
            let goto = compiled.GotoTable.[i].[j] 
            match goto with 
            | None -> ()
            | Some n -> 
                gotoTableCurrIndex := !gotoTableCurrIndex + 1;
                outputCodedUInt16 os i;
                outputCodedUInt16 os n;
      fprintfn os "|]" ;
      outputUInt16Array os "sparseGotoTableRowOffsets" gotoIndexes
  end;

  begin 
    fprintf os "let actionTableElements = [|" ;
    let actionIndexes = Array.create compiled.ActionTable.Length 0 
    
    let actionTableCurrIndex = ref 0 
    for i = 0 to nStates-1 do 
        actionIndexes.[i] <- !actionTableCurrIndex;
        let actions = compiled.ActionTable.[i] 
        let terminalsByAction = new Dictionary<_,int list>(10) 
        let countPerAction = new Dictionary<_,_>(10) 
        for terminal = 0 to preprocessed.Terminals.Length - 1 do  
              let action = match actions with ImmediateAction actions -> actions | LookaheadActions actions ->  actions.[terminal] 
              if terminalsByAction.ContainsKey action then 
                  terminalsByAction.[action] <- terminal :: terminalsByAction.[action] ;
              else
                  terminalsByAction.[action] <- [terminal];
              if countPerAction.ContainsKey action then 
                countPerAction.[action] <- countPerAction.[action]+1
              else 
                countPerAction.[action] <- 1

        let mostCommonAction = 
            let mostCommon = ref Error 
            let max = ref 0 
            for (KeyValue(x,y)) in countPerAction do 
                if y > !max then (mostCommon := x; max := y)
            !mostCommon 

        (* Count the number of entries in the association table. *)
        let count = ref 0 
        for (KeyValue(action,terminals)) in terminalsByAction do 
            for terminals  in terminals do 
               if action <> mostCommonAction then  
                   incr count;
        
        (* Write the head of the table (i.e. the number of entries and the default value) *)
        actionTableCurrIndex := !actionTableCurrIndex + 1;
        outputCodedUInt16 os !count;
        outputCodedUInt16 os (actionCoding mostCommonAction);
        
        (* Write the pairs of entries in incremental order by key *)
        (* This lets us implement the lookup by a binary chop. *)
        for terminal = 0 to preprocessed.Terminals.Length - 1 do  
            let action = match actions with ImmediateAction actions -> actions | LookaheadActions actions ->  actions.[terminal]
            if action <> mostCommonAction then  (
                actionTableCurrIndex := !actionTableCurrIndex + 1;
                outputCodedUInt16 os terminal;
                outputCodedUInt16 os (actionCoding action);
            );
    fprintfn os "|]" ;
    (* Output offsets into actions table where the actions for a particular nonterminal begin *)
    fprintf os "let actionTableRowOffsets = [|" ;
    for j = 0 to compiled.ActionTable.Length - 1 do  
        fprintf os "%a" outputCodedUInt16 actionIndexes.[j];
    fprintfn os "|]" ;

  end;
  outputUInt16Array os "reductionSymbolCounts" (Array.map (fun (prod : CompiledProduction) -> prod.BodySymbolIndexes.Length) compiled.Productions)
  outputUInt16Array os "productionToNonTerminalTable" (Array.map (fun (prod : CompiledProduction) -> prod.HeadNonTerminalIndex) compiled.Productions)
  
  let getType nt = if types.ContainsKey nt then  types.[nt] else "'"+nt 
  begin 
      fprintf os "let reductions =" ;
      fprintfn os "    [| " ;
      for prod in preprocessed.Productions do 
          fprintfn os "        (fun (parseState : %s.IParseState) ->"  parslib
          prod.Body |> Array.iteri (fun i sym -> 
              let tyopt =
                  if Array.contains sym preprocessed.NonTerminals then
                        Some (getType sym)
                  else
                      if tokens.ContainsKey sym then 
                        tokens.[sym]
                      else None
              match tyopt with 
              | Some ty -> fprintfn os "            let _%d = (let data = parseState.GetInput(%d) in (Microsoft.FSharp.Core.Operators.unbox data : %s)) in" (i+1) (i+1) ty
              | None -> ())
          fprintfn os "            Microsoft.FSharp.Core.Operators.box" 
          fprintfn os "                (";
          fprintfn os "                   (";
          match prod.Code with 
          | Some (code,_) -> 
              let dollar = ref false in 
              let c = code |> String.collect (fun c -> 
                  if not !dollar && c = '$' then (dollar := true; "")
                  elif !dollar && c >= '0' && c <= '9' then (dollar := false; "_"+new System.String(c,1))
                  elif !dollar then (dollar := false; "$"+new System.String(c,1))
                  else new System.String(c,1))
              let lines = c.Split([| '\r'; '\n' |], System.StringSplitOptions.RemoveEmptyEntries);
              for line in lines do 
                  fprintfn os "                     %s" line;
              if !dollar then os.Write '$'
          | None -> 
              fprintfn os "                      failwith \"unreachable\""
          fprintfn os "                   )";
          fprintfn os "                 : %s));" (if types.ContainsKey prod.Head then  types.[prod.Head] else "'" + prod.Head);
      done;
      fprintfn os "|]" ;
  end;
  fprintfn os "let tables = %s.Tables(reductions, endOfInputTag, tagOfToken, dataOfToken, actionTableElements, actionTableRowOffsets, reductionSymbolCounts, gotos, sparseGotoTableRowOffsets, productionToNonTerminalTable)" parslib

  for (id,startState) in Seq.zip spec.StartSymbols compiled.StartStates do
        if not (types.ContainsKey id) then 
          failwith ("a %type declaration is required for for start token "+id);
        let ty = types.[id] in 
        fprintfn os "let %s lexer lexbuf : %s = unbox (tables.Interpret(lexer, lexbuf, %d))" id ty startState

  for id in spec.StartSymbols do
      if not (types.ContainsKey id) then 
        failwith ("a %type declaration is required for start token "+id);

  Option.iter (fun (f : TextWriter) -> f.Close()) logf

let _ = 
    try main()
    with e -> 
      eprintf "FSYACC: error FSY000: %s" (match e with Failure s -> s | e -> e.Message);
      exit 1

