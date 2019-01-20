module YaccOutput

open System
open System.Collections.Generic
open System.IO
open System.Drawing
open System.Drawing.Imaging
open Printf
open Hashtable
open Syntax
open Preprocess
open Compile

let outputPrecInfo f p = 
    match p with 
    | Some (assoc, n, symbol) -> fprintf f " (%d %s %s)" n (stringOfAssoc assoc) symbol
    | None  -> ()

let stringOfPrecInfo p = 
    match p with 
    | Some (assoc,n) -> sprintf "(%d %s)" n (stringOfAssoc assoc)
    | None  -> ""
    
let outputTable (f : TextWriter) (indent : int) (rows : (string * int) [] []) =
    let cols = if rows.Length > 0 then Array.max (Array.map Array.length rows) else 0
    let maxWidths = Array.create cols 0
    for row in rows do
        for i = 0 to row.Length - 1 do
            maxWidths.[i] <- max maxWidths.[i] (snd row.[i])
    for row in rows do
        f.Write(String(' ', indent))
        for i = 0 to row.Length - 1 do
            if i > 0 then f.Write(' ')
            f.Write(fst row.[i])
            f.Write(String(' ', maxWidths.[i] - (snd row.[i])))
        f.WriteLine()

let outputCompilationReport (path : string) (spec : Preprocessed) (comp : Compiled) =
    use f = (File.CreateText path) :> TextWriter
    fprintfn f "<pre>"
    fprintfn f ""
    fprintfn f "------------------------";
    fprintfn f "states = ";
    fprintfn f "";
    for i = 0 to comp.Kernels.Length - 1 do
        fprintfn f "<div id=\"s%d\">state %d:</div>" i i
            
        fprintfn f "  items:"
        for item in comp.Kernels.[i] do
            let syms = ResizeArray(spec.Productions.[item.ProductionIndex].Body)
            let mark = if item.DotIndex < syms.Count then "\u25CF" else "\u25A0"
            syms.Insert(item.DotIndex, mark)
            fprintf f "    %s -&gt; %s" (spec.Productions.[item.ProductionIndex].Head) (String.Join(" ", syms))
            fprintfn f "%a" outputPrecInfo spec.Productions.[item.ProductionIndex].PrecedenceInfo
        fprintfn f ""

        let row = comp.ActionTable.[i]
        fprintfn f "  lookahead actions:"
        let rows = ResizeArray()
        for j = 0 to row.LookaheadActions.Length - 1 do
            let action = row.LookaheadActions.[j]
            if action <> None then
                let term, prec = spec.Terminals.[j]
                let precText = stringOfPrecInfo prec
                let termText = (if precText.Length = 0 then term else term + " " + precText) + ":"

                let actionText =
                    match action.Value with 
                    | Shift n -> (sprintf "shift <a href=\"#s%d\">%d</a>" n n, (6 + n.ToString().Length))
                    | Reduce prodIdx ->
                        let s = sprintf "reduce %s -&gt; %s" (spec.NonTerminals.[comp.Productions.[prodIdx].HeadNonTerminalIndex]) (String.Join(" ", spec.Productions.[prodIdx].Body))
                        (s, s.Length - 4)
                    | Error -> ("error", 5)
                    | Accept -> ("accept", 6)
                    
                rows.Add([| (termText, termText.Length); actionText |])
        outputTable f 4 (rows.ToArray())
        fprintfn f ""

        fprintfn f "  default action:"
        let actionText =
            match comp.ActionTable.[i].DefaultAction with 
            | Shift n -> (sprintf "shift <a href=\"#s%d\">%d</a>" n n, (6 + n.ToString().Length))
            | Reduce prodIdx ->
                let s = sprintf "reduce %s -&gt; %s" (spec.NonTerminals.[comp.Productions.[prodIdx].HeadNonTerminalIndex]) (String.Join(" ", spec.Productions.[prodIdx].Body))
                (s, s.Length - 4)
            | Error -> ("error", 5)
            | Accept -> ("accept", 6)
        fprintfn f "    %s (for %d lookaheads)" (fst actionText) (spec.Terminals.Length - comp.ActionTable.[i].LookaheadActions.Length)
        fprintfn f ""

        fprintfn f "  gotos:"
        for j = 0 to comp.GotoTable.[i].Length - 1 do
            match comp.GotoTable.[i].[j] with
            | Some st -> fprintfn f "    %s: <a href=\"#s%d\">%d</a>" spec.NonTerminals.[j] st st
            | None -> ()
        fprintfn f ""
            
    fprintfn f "startStates = %s" (String.Join(";", (Array.map string comp.StartStates)));
    fprintfn f "------------------------"
    fprintfn f "</pre>"

let randomLightColor (hashBase : int) (key : int) =
    let cutoff = 0.3
    let h = hashBase * key
    let f x = int (float 0xFF * (cutoff + (1.0 - cutoff) * float (0x3FF &&& x) / float 0x400))
    let r = f h
    let g = f (h >>> 10)
    let b = f (h >>> 20)
    Color.FromArgb(0xFF, r, g, b)

let outputTableImages (path : string) (p : Preprocessed) (c : Compiled)  =
    use actionTableBmp = new Bitmap(p.Terminals.Length, c.States.Length)
    let colorOfAction (x : Action) =
        match x with
        | Error -> Color.Black
        | Accept -> Color.White
        | Shift code ->  randomLightColor 5999471 code //Color.FromArgb(0xFF000000 ||| (5999471 * code))
        | Reduce code -> randomLightColor 7199369 code //Color.FromArgb(0xFF000000 ||| (7199369 * code))
    for y = 0 to actionTableBmp.Height - 1 do
        let row = c.ActionTable.[y]
        for x = 0 to actionTableBmp.Width - 1 do
            let color =
                match row.LookaheadActions.[x] with
                | Some action  -> colorOfAction action
                | None -> Color.Black
            actionTableBmp.SetPixel(x, y, color)
    use scaled = Hashtable.scaleImage 4 actionTableBmp
    scaled.Save(path + "-actionTable.png", ImageFormat.Png)

    use gotoTableBmp = new Bitmap(p.NonTerminals.Length, c.States.Length)
    let colorOfGoto x =
        match x with
        | None -> Color.Black
        | Some code -> randomLightColor 5999471 code //Color.FromArgb(0xFF000000 ||| (5999471 * code))
    for i = 0 to gotoTableBmp.Width - 1 do
        for j = 0 to gotoTableBmp.Height - 1 do
            gotoTableBmp.SetPixel(i, j, colorOfGoto c.GotoTable.[j].[i])
    use scaled = Hashtable.scaleImage 4 gotoTableBmp
    scaled.Save(path + "-gotoTable.png", ImageFormat.Png)

let outputUInt16Array (os : TextWriter) (name : string) (ary : int array) =
    fprintf os "let %s = [|" name
    for i = 0 to ary.Length - 1 do  
        if i <> 0 then
            fprintf os "; "
        fprintf os "%dus" ary.[i]
    fprintfn os "|]"

let outputCodedUInt16 (os: TextWriter) (n:int) = 
  os.Write n;
  os.Write "us; "
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

let actionCoding2 action =
  match action with 
  | Accept -> Int16.MaxValue
  | Shift n -> int16 n
  | Reduce n -> ~~~ (int16 n)
  | Error -> Int16.MinValue

let outputParser (output : string) (modname : string) (lexlib : string) (parslib : string) (code : string) (spec : ParserSpec) (preprocessed : Preprocessed) (compiled : Compiled) (actionHashtable : Hashtable) (gotoHashtable : Hashtable) =
  use os = (File.CreateText output :> TextWriter)

  fprintfn os "// Implementation file for parser generated by fsyacc"
  fprintfn os "module %s" modname
  fprintfn os "#nowarn \"64\""
  fprintfn os "open %s" lexlib;
  fprintfn os "%s" code;
  fprintfn os "type token = ";
  for id, typ in spec.Tokens do 
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
        let row = compiled.ActionTable.[i]
        let ary = Array.map (fun x -> match x with None -> row.DefaultAction | Some x -> x ) row.LookaheadActions

        let terminalsByAction = new Dictionary<_,int list>(10) 
        let countPerAction = new Dictionary<_,_>(10) 
        for terminal = 0 to preprocessed.Terminals.Length - 1 do  
              let action = ary.[terminal] 
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
            let action = ary.[terminal]
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
  
  fprintfn os "let maxProductionBodyLength = %d" (Array.max (Array.map (fun (prod : CompiledProduction) -> prod.BodySymbolIndexes.Length) compiled.Productions))

  outputHashtable os "actionTable" actionHashtable
  outputInt16Array os "actionTable_defaultActions" (Array.map (fun (row : ActionTableRow) -> int (actionCoding2 row.DefaultAction)) compiled.ActionTable)
  outputHashtable os "gotoTable" gotoHashtable


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

  fprintfn os "let terminalsCount = %d" preprocessed.Terminals.Length  
  fprintfn os "let nonTerminalsCount = %d" preprocessed.NonTerminals.Length

  fprintfn os "let tables = %s.Tables(reductions, endOfInputTag, tagOfToken, dataOfToken, actionTableElements, actionTableRowOffsets, reductionSymbolCounts, gotos, sparseGotoTableRowOffsets, productionToNonTerminalTable, maxProductionBodyLength, gotoTable_buckets, gotoTable_entries, nonTerminalsCount, actionTable_buckets, actionTable_entries, actionTable_defaultActions, terminalsCount)" parslib

  for (id, startState) in Seq.zip spec.StartSymbols compiled.StartStates do
        let ty = types.[id]
        fprintfn os "let %s lexer lexbuf : %s = unbox (tables.Interpret(lexer, lexbuf, %d))" id ty startState    
  ()