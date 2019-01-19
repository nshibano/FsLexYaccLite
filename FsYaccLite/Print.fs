module Print

open System
open System.Collections.Generic
open System.IO
open System.Drawing
open System.Drawing.Imaging
open Printf
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

let outputCompilationReport (f : TextWriter) (spec : Preprocessed) (comp : Compiled) =

    printfn  "writing tables to log"
    stdout.Flush()
        
    fprintfn f ""

    fprintfn f "FIRST sets:"

    let rows = List()

    for nonTerminalIndex = 0 to spec.NonTerminals.Length - 1 do
        let rowA = spec.NonTerminals.[nonTerminalIndex] + ":"
        let firstSet = sortedArrayOfHashSet(comp.FirstSets.[NonTerminalIndex nonTerminalIndex])
        let itemStrings = Array.map (fun item -> match item with Epsilon -> "ε" | terminalIndex -> fst spec.Terminals.[terminalIndex]) firstSet
        let rowB = String.Join(" ", itemStrings)
        rows.Add([| (rowA, rowA.Length); (rowB, rowB.Length) |])

    outputTable f 2 (rows.ToArray())

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
            let k, action = row.LookaheadActions.[j]
            if action <> Error || true then
                let term, prec = spec.Terminals.[k]
                let precText = stringOfPrecInfo prec
                let termText = (if precText.Length = 0 then term else term + " " + precText) + ":"

                let actionText =
                    match action with 
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
//            actionTableBmp.SetPixel(x, y, colorOfAction row.DefaultAction)
            actionTableBmp.SetPixel(x, y, Color.Black)
        for (x, action) in row.LookaheadActions do
            actionTableBmp.SetPixel(x, y, colorOfAction action)
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
