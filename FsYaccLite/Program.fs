open System
open System.IO 
open System.Collections.Generic
open Printf
open FsLexYaccLite.Lexing
open FsLexYaccLite.Common.Arg
open Syntax
open Preprocess
open Compile

//------------------------------------------------------------------
// This code is duplicated from Microsoft.FSharp.Compiler.UnicodeLexing

type Lexbuf =  LexBuffer

let UnicodeFileAsLexbuf (filename,codePage : int option) =
    let lexbuf = LexBuffer.FromString(File.ReadAllText(filename))
    lexbuf.EndPos <- Position.FirstLine(filename);
    lexbuf

//------------------------------------------------------------------
// This is the program proper

let input = ref None
let modname= ref None
let internal_module = ref false
let opens= ref []
let out = ref None
let tokenize = ref false
let compat = ref false
let log = ref false
let light = ref None
let inputCodePage = ref None
let mutable lexlib = "FsLexYaccLite.Parsing"
let mutable parslib = "FsLexYaccLite.Parsing"

let usage =
  [ ("-o", StringArg (fun s -> out := Some s), "Name the output file.");
    ("-v", UnitArg (fun () -> log := true), "Produce a listing file."); 
    ("--module", StringArg (fun s -> modname := Some s), "Define the F# module name to host the generated parser."); 
    ("--internal", UnitArg (fun () -> internal_module := true), "Generate an internal module");
    ("--open", StringArg (fun s -> opens := !opens @ [s]), "Add the given module to the list of those to open in both the generated signature and implementation."); 
    ("--light", UnitArg (fun () ->  light := Some true), "(ignored)");
    ("--light-off", UnitArg (fun () ->  light := Some false), "Add #light \"off\" to the top of the generated file");
    ("--ml-compatibility", UnitArg (fun () -> compat := true), "Support the use of the global state from the 'Parsing' module in FSharp.PowerPack.dll."); 
    ("--tokens", UnitArg (fun () -> tokenize := true), "Simply tokenize the specification file itself."); 
    ("--lexlib", StringArg (fun s ->  lexlib <- s), "Specify the namespace for the implementation of the lexer (default: Microsoft.FSharp.Text.Lexing)");
    ("--parslib", StringArg (fun s ->  parslib <- s), "Specify the namespace for the implementation of the parser table interpreter (default: Microsoft.FSharp.Text.Parsing)");
    ("--codepage", IntArg (fun i -> inputCodePage := Some i), "Assume input lexer specification file is encoded with the given codepage.")]

let _ = parseCommandLineArgs usage (fun x -> match !input with Some _ -> failwith "more than one input given" | None -> input := Some x) "fsyacc <filename>"

let output_int (os: #TextWriter) (n:int) = os.Write(string n)

let outputCodedUInt16 (os: #TextWriter)  (n:int) = 
  os.Write n;
  os.Write "us; ";

let shiftFlag = 0x0000
let reduceFlag = 0x4000
let errorFlag = 0x8000
let acceptFlag = 0xc000
let actionMask = 0xc000

let anyMarker = 0xffff

let actionCoding action  =
  match action with 
  | Accept -> acceptFlag
  | Shift n -> shiftFlag ||| n
  | Reduce n -> reduceFlag ||| n
  | Error -> errorFlag 

let main() = 
  let filename = (match !input with Some x -> x | None -> failwith "no input given") in 

  let spec = 
      let lexbuf = UnicodeFileAsLexbuf(filename, !inputCodePage) 

      try 
        if !tokenize then begin 
          while true do 
            printf "tokenize - getting one token";
            let t = Lexer.token lexbuf in 
            (*F# printf "tokenize - got %s" (Parser.token_to_string t); F#*)
            if t = Parser.EOF then exit 0;
          done;
        end;
    
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

  let output = match !out with Some x -> x | _ -> chop_extension filename + (if checkSuffix filename ".mly" then ".ml" else ".fs") in
  let outputi = match !out with Some x -> chop_extension x + (if checkSuffix x ".ml" then ".mli" else ".fsi") | _ -> chop_extension filename + (if checkSuffix filename ".mly" then ".mli" else ".fsi") in
  let outputo = 
      if !log then Some (match !out with Some x -> chop_extension x + ".fsyacc.html" | _ -> chop_extension filename + ".fsyacc.html") 
      else None 

  use os = (File.CreateText output :> TextWriter)

  let lineCountOutput = ref 0
  let cos = (os,lineCountOutput)
  let cprintf (os:TextWriter,lineCount) fmt = Printf.fprintf os fmt
  let cprintfn (os:TextWriter,lineCount) fmt = Printf.kfprintf (fun () -> incr lineCount; os.WriteLine()) os fmt

  let logf = Option.map (fun path -> File.CreateText path :> TextWriter) outputo
  Option.iter (fun f ->
    fprintfn f "<pre>"
    fprintfn f "Output file describing compiled parser placed in %s and %s" output outputi) logf

  printfn "building tables"; 
  let preprocessed = processParserSpecAst spec
  let compiled = compile preprocessed
  Option.iter (fun f -> Print.outputCompilationReport f preprocessed compiled) logf
  if !log then
    Print.outputTableImages filename preprocessed compiled 
  Option.iter (fun f -> fprintfn f "</pre>") logf

  let (code,pos) = spec.Header 
  printfn "%d states" compiled.States.Length; 
  printfn "%d nonterminals" compiled.GotoTable.[0].Length; 
  printfn "%d terminals" preprocessed.Terminals.Length
  printfn "%d productions" compiled.Productions.Length; 
  printfn "#rows in action table: %d" compiled.ActionTable.Length; 


  cprintfn cos "// Implementation file for parser generated by fsyacc";

  if (!light = Some(false)) || (!light = None && checkSuffix output ".ml") then
      cprintfn cos "#light \"off\"";

  match !modname with 
  | None ->
        let moduleName =
            let s = Path.GetFileNameWithoutExtension(filename)
            String(Char.ToUpperInvariant s.[0], 1) + s.Substring(1)
        cprintfn cos "module %s" moduleName
  | Some s -> 
      match !internal_module with
      | true ->
          cprintfn cos "module internal %s" s;
      | false ->
          cprintfn cos "module %s" s;  
  
  cprintfn cos "#nowarn \"64\""

  for s in !opens do
      cprintfn cos "open %s" s;

  cprintfn cos "open %s" lexlib;
  if !compat then 
      cprintfn cos "open Microsoft.FSharp.Compatibility.OCaml.Parsing";

  cprintfn cos "%s" code;
  lineCountOutput := !lineCountOutput + code.Replace("\r","").Split([| '\n' |]).Length;

  cprintfn cos "// This type is the type of tokens accepted by the parser";
  for out in [cos] do
      cprintfn out "type token = ";
      for id,typ in spec.Tokens do 
          match typ with
          | None -> cprintfn out "  | %s" id
          | Some ty -> cprintfn out "  | %s of (%s)" id ty; 

  cprintfn cos "// This type is used to give symbolic names to token indexes, useful for error messages";
  for out in [cos] do
      cprintfn out "type tokenId = ";
      for id,typ in spec.Tokens do 
          cprintfn out "    | TOKEN_%s" id;
      cprintfn out "    | TOKEN_end_of_input";
      cprintfn out "    | TOKEN_error";

  cprintfn cos "// This type is used to give symbolic names to token indexes, useful for error messages";
  for out in [cos] do
      cprintfn out "type nonTerminalId = ";
      for nt in preprocessed.NonTerminals do 
          cprintfn out "    | NONTERM_%s" nt;

  cprintfn cos "";
  cprintfn cos "// This function maps tokens to integer indexes";
  cprintfn cos "let tagOfToken (t:token) = ";
  cprintfn cos "  match t with";
  spec.Tokens |> List.iteri (fun i (id,typ) -> 
      cprintfn cos "  | %s %s -> %d " id (match typ with Some _ -> "_" | None -> "") i);

  cprintfn cos "";
  cprintfn cos "// This function maps integer indexes to symbolic token ids";
  cprintfn cos "let tokenTagToTokenId (tokenIdx:int) = ";
  cprintfn cos "  match tokenIdx with";
  spec.Tokens |> List.iteri (fun i (id,typ) -> 
      cprintfn cos "  | %d -> TOKEN_%s " i id)
  cprintfn cos "  | %d -> TOKEN_end_of_input" compiled.EndOfInputTerminalIndex
  cprintfn cos "  | %d -> TOKEN_error" compiled.ErrorTerminalIndex
  cprintfn cos "  | _ -> failwith \"tokenTagToTokenId: bad token\""


  cprintfn cos "";
  cprintfn cos "/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production";
  cprintfn cos "let prodIdxToNonTerminal (prodIdx:int) = ";
  cprintfn cos "  match prodIdx with";
  preprocessed.Productions |> Array.iteri (fun i prod -> 
      cprintfn cos "    | %d -> NONTERM_%s " i prod.Head);
  cprintfn cos "    | _ -> failwith \"prodIdxToNonTerminal: bad production index\""

  cprintfn cos "";
  cprintfn cos "let _fsyacc_endOfInputTag = %d " compiled.EndOfInputTerminalIndex;
  cprintfn cos "";
  cprintfn cos "// This function gets the name of a token as a string";
  cprintfn cos "let token_to_string (t:token) = ";
  cprintfn cos "  match t with ";
  spec.Tokens |> List.iteri (fun i (id,typ) -> 
      cprintfn cos "  | %s %s -> \"%s\" " id (match typ with Some _ -> "_" | None -> "") id);

  cprintfn cos "";
  cprintfn cos "// This function gets the data carried by a token as an object";
  cprintfn cos "let _fsyacc_dataOfToken (t:token) = ";
  cprintfn cos "  match t with ";

  for (id,typ) in spec.Tokens do
      cprintfn cos "  | %s %s -> %s " 
        id
        (match typ with Some _ -> "_fsyacc_x" | None -> "")
        (match typ with Some _ -> "Microsoft.FSharp.Core.Operators.box _fsyacc_x" | None -> "(null : System.Object)")

  for (key,_) in spec.Types |> Seq.countBy fst |> Seq.filter (fun (_,n) -> n > 1)  do
        failwithf "%s is given multiple %%type declarations" key;
    
  for (key,_) in spec.Tokens |> Seq.countBy fst |> Seq.filter (fun (_,n) -> n > 1)  do
        failwithf "%s is given %%token declarations" key
    
  let types = Map.ofList spec.Types 
  let tokens = Map.ofList spec.Tokens 
  
  let nStates = compiled.States.Length 
  begin 
      cprintf cos "let _fsyacc_gotos = [| " ;
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
      cprintfn cos "|]" ;
      (* Output offsets into gotos table where the gotos for a particular nonterminal begin *)
      cprintf cos "let _fsyacc_sparseGotoTableRowOffsets = [|" ;
      for j = 0 to numGotoNonTerminals-1 do  
          outputCodedUInt16 os gotoIndexes.[j];
      cprintfn cos "|]" ;
  end;

  begin 
      cprintf cos "let _fsyacc_stateToProdIdxsTableElements = [| " ;
      let indexes = Array.create compiled.States.Length 0 
      let currIndex = ref 0 
      for j = 0 to compiled.States.Length - 1 do
          let state = compiled.States.[j]
          indexes.[j] <- !currIndex;

          (* Write the head of the table (i.e. the number of entries) *)
          outputCodedUInt16 os compiled.States.Length;
          currIndex := !currIndex + compiled.States.Length + 1;
          
          (* Write the pairs of entries in incremental order by key *)
          (* This lets us implement the lookup by a binary chop. *)
          for prodIdx in state do
                outputCodedUInt16 os prodIdx;
      cprintfn cos "|]" ;
      (* Output offsets into gotos table where the gotos for a particular nonterminal begin *)
      cprintf cos "let _fsyacc_stateToProdIdxsTableRowOffsets = [|" ;
      for idx in indexes do 
          outputCodedUInt16 os idx;
      cprintfn cos "|]" ;
  end;

  begin 
    let numActionRows = (Array.length compiled.ActionTable) 
    //let maxActionColumns = Array.length compiled.ActionTable.[0] 
    cprintfn cos "let _fsyacc_action_rows = %d" numActionRows;
    cprintf cos "let _fsyacc_actionTableElements = [|" ;
    let actionIndexes = Array.create numActionRows 0 
    
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
    cprintfn cos "|]" ;
    (* Output offsets into actions table where the actions for a particular nonterminal begin *)
    cprintf cos "let _fsyacc_actionTableRowOffsets = [|" ;
    for j = 0 to numActionRows-1 do  
        cprintf cos "%a" outputCodedUInt16 actionIndexes.[j];
    cprintfn cos "|]" ;

  end;
  begin 
      cprintf cos "let _fsyacc_reductionSymbolCounts = [|" ;
      for prod in preprocessed.Productions do 
          cprintf cos "%a" outputCodedUInt16 prod.Body.Length;
      cprintfn cos "|]" ;
  end;
  begin 
      cprintf cos "let _fsyacc_productionToNonTerminalTable = [|" ;
      for prod in compiled.Productions do 
          cprintf cos "%a" outputCodedUInt16 prod.HeadNonTerminalIndex;
      cprintfn cos "|]" ;
  end;
  
  let getType nt = if types.ContainsKey nt then  types.[nt] else "'"+nt 
  begin 
      cprintf cos "let _fsyacc_reductions =" ;
      cprintfn cos "    [| " ;
      for prod in preprocessed.Productions do 
          //cprintfn cos "# %d \"%s\"" !lineCountOutput output;
          cprintfn cos "        (fun (parseState : %s.IParseState) ->"  parslib
          if !compat then 
              cprintfn cos "            Parsing.set_parse_state parseState;"
          prod.Body |> Array.iteri (fun i sym -> 
              let tyopt =
                  if Array.contains sym preprocessed.NonTerminals then
                        Some (getType sym)
                  else
                      if tokens.ContainsKey sym then 
                        tokens.[sym]
                      else None
              match tyopt with 
              | Some ty -> cprintfn cos "            let _%d = (let data = parseState.GetInput(%d) in (Microsoft.FSharp.Core.Operators.unbox data : %s)) in" (i+1) (i+1) ty
              | None -> ())
          cprintfn cos "            Microsoft.FSharp.Core.Operators.box" 
          cprintfn cos "                (";
          cprintfn cos "                   (";
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
                  cprintfn cos "                     %s" line;
              if !dollar then os.Write '$'
          | None -> 
              cprintfn cos "                      raise (%s.Accept(Microsoft.FSharp.Core.Operators.box _1))" parslib
          cprintfn cos "                   )";
          cprintfn cos "                 : %s));" (if types.ContainsKey prod.Head then  types.[prod.Head] else "'" + prod.Head);
      done;
      cprintfn cos "|]" ;
  end;
  cprintfn cos "let tables = %s.Tables(_fsyacc_reductions, _fsyacc_endOfInputTag, tagOfToken, _fsyacc_dataOfToken, _fsyacc_actionTableElements, _fsyacc_actionTableRowOffsets, _fsyacc_reductionSymbolCounts, _fsyacc_gotos, _fsyacc_sparseGotoTableRowOffsets, _fsyacc_productionToNonTerminalTable)" parslib

  for (id,startState) in Seq.zip spec.StartSymbols compiled.StartStates do
        if not (types.ContainsKey id) then 
          failwith ("a %type declaration is required for for start token "+id);
        let ty = types.[id] in 
        cprintfn cos "let %s lexer lexbuf : %s =" id ty;
        cprintfn cos "    Microsoft.FSharp.Core.Operators.unbox (tables.Interpret(lexer, lexbuf, %d))" startState

  for id in spec.StartSymbols do
      if not (types.ContainsKey id) then 
        failwith ("a %type declaration is required for start token "+id);

  Option.iter (fun (f : TextWriter) -> f.Close()) logf

let _ = 
    try main()
    with e -> 
      eprintf "FSYACC: error FSY000: %s" (match e with Failure s -> s | e -> e.Message);
      exit 1
