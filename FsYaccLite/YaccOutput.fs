﻿module YaccOutput

open System
open System.Collections.Generic
open System.IO
open System.Text
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

let outputParser (output : string) (modname : string) (parslib : string) (code : string) (spec : ParserSpec) (preprocessed : Preprocessed) (compiled : Compiled) (actionHashtable : Hashtable) (gotoHashtable : Hashtable) =
  use os = (File.CreateText output :> TextWriter)

  fprintfn os "// Implementation file for parser generated by fsyacc"
  fprintfn os "module %s" modname
  fprintfn os "#nowarn \"64\""
  fprintfn os "%s" code;
  fprintfn os "type token = "
  for id, typ in preprocessed.Tokens do 
    match typ with
    | None -> fprintfn os "  | %s" id
    | Some ty -> fprintfn os "  | %s of %s" id ty; 

  fprintfn os "";
  fprintfn os "let tagOfToken (t : token) = ";
  fprintfn os "  match t with";
  preprocessed.Tokens |> Array.iteri (fun i (id,typ) -> 
      fprintfn os "  | %s %s -> %d " id (match typ with Some _ -> "_" | None -> "") i);
  fprintfn os "";
  fprintfn os "let endOfInputTag = %d " compiled.EndOfInputTerminalIndex;
  fprintfn os "";
  fprintfn os "let dataOfToken (t : token) : obj = ";
  fprintfn os "  match t with ";

  for (id,typ) in preprocessed.Tokens do
      fprintfn os "  | %s %s -> %s " 
        id
        (match typ with Some _ -> "x" | None -> "")
        (match typ with Some _ -> "box x" | None -> "null")
    
  for (key,_) in preprocessed.Tokens |> Seq.countBy fst |> Seq.filter (fun (_,n) -> n > 1)  do
        failwithf "%s is given %%token declarations" key
  fprintfn os ""
  
  outputUInt16Array os "reductionSymbolCounts" (Array.map (fun (prod : CompiledProduction) -> prod.BodySymbolIndexes.Length) compiled.Productions)
  outputUInt16Array os "productionToNonTerminalTable" (Array.map (fun (prod : CompiledProduction) -> prod.HeadNonTerminalIndex) compiled.Productions)
  
  fprintfn os "let maxProductionBodyLength = %d" (Array.max (Array.map (fun (prod : CompiledProduction) -> prod.BodySymbolIndexes.Length) compiled.Productions))

  outputHashtable os "actionTable" actionHashtable
  outputInt16Array os "actionTable_defaultActions" (Array.map (fun (row : ActionTableRow) -> int (actionCoding2 row.DefaultAction)) compiled.ActionTable)
  outputHashtable os "gotoTable" gotoHashtable

  let typeOfNonTerminal = preprocessed.Types 
  let typeOfToken = Map.ofArray preprocessed.Tokens 
  let getType nt = if typeOfNonTerminal.ContainsKey nt then typeOfNonTerminal.[nt] else "'" + nt 
  
  begin 
      fprintfn os "let reductions (i : int) (parseState : FsLexYaccLiteRuntime.IParseState) ="
      fprintfn os "    match i with"
      for i = 0 to preprocessed.Productions.Length - 1 do
          let prod = preprocessed.Productions.[i]
          fprintfn os "    | %d ->" i
          Array.iteri (fun i sym -> 
              let tyopt =
                  if Array.contains sym preprocessed.NonTerminals then
                        Some (getType sym)
                  elif typeOfToken.ContainsKey sym then 
                        typeOfToken.[sym]
                  else None
              match tyopt with 
              | Some ty -> fprintfn os "        let _%d = unbox<%s> (parseState.GetInput(%d))" (i+1) ty (i+1)
              | None -> ()) prod.Body
          let code =
                if isNull prod.Code then
                    "failwith \"unreachable\""
                elif String.IsNullOrWhiteSpace prod.Code then
                    "()"
                else
                    let code = prod.Code
                    let sb = StringBuilder()
                    let mutable pos = 0
                    while pos < code.Length do
                        if code.[pos] = '$' then
                            pos <- pos + 1
                            let st = pos
                            while pos < code.Length && '0' <= code.[pos] && code.[pos] <= '9' do
                                pos <- pos + 1
                            if pos = st then
                                failwith "Dollar sign must be followed by a number"
                            let i = Int32.Parse(code.Substring(st, pos - st))
                            bprintf sb "_%d" i
                        else
                            sb.Append(code.[pos]) |> ignore
                            pos <- pos + 1
                    sb.ToString()
          let linesCount = code.Split([| "\r"; "\n" |], StringSplitOptions.None).Length
          if linesCount > 1 then
              fprintfn os "        let res ="
              Output.outputCode os 12 code
          else
              fprintfn os "        let res = %s" (code.Trim(' ', '\t'))
          fprintfn os "        box<%s> res" (getType prod.Head)
      fprintfn os "    | _ -> failwith \"unreachable\""
  end;

  fprintfn os "let terminalsCount = %d" preprocessed.Terminals.Length  
  fprintfn os "let nonTerminalsCount = %d" preprocessed.NonTerminals.Length

  fprintfn os "let tables = %s.ParseTables(reductions, endOfInputTag, tagOfToken, dataOfToken, reductionSymbolCounts, productionToNonTerminalTable, maxProductionBodyLength, gotoTable_buckets, gotoTable_entries, nonTerminalsCount, actionTable_buckets, actionTable_entries, actionTable_defaultActions, terminalsCount)" parslib

  for (id, startState) in Seq.zip preprocessed.OriginalStartSymbols compiled.StartStates do
        fprintfn os "let %s lexer lexbuf = unbox<%s> (tables.Interpret(lexer, lexbuf, %d))" id typeOfNonTerminal.[id] startState
