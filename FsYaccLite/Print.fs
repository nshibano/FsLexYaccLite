module FsLexYacc.FsYacc.Print

open System
open System.Collections.Generic
open System.IO
open System.Diagnostics
open System.Drawing

open Printf

open Syntax
open Preprocess
open Compile

let outputTableImages (path : string) (spec : ParserSpec) (pre : ProcessedParserSpec) (comp : CompiledTable)  =
    let actionTableBmp = new Bitmap(pre.Terminals.Length, comp.States.Length)
    let colorOfAction (x : Action) =
        match x with
        | Error -> Color.Black
        | Accept -> Color.White
        | Shift code ->  Color.FromArgb(0xFF000000 ||| (5999471 * code))
        | Reduce code -> Color.FromArgb(0xFF000000 ||| (7199369 * code))
    for i = 0 to actionTableBmp.Width - 1 do
        for j = 0 to actionTableBmp.Height - 1 do
            actionTableBmp.SetPixel(i, j, colorOfAction (comp.ActionTable.[j].[i]))
    actionTableBmp.Save(path + "-actionTable.bmp")

    let gotoTableBmp = new Bitmap(pre.NonTerminals.Length, comp.States.Length)
    let colorOfGoto x =
        match x with
        | None -> Color.Black
        | Some code -> Color.FromArgb(0xFF000000 ||| (5999471 * code))
    for i = 0 to gotoTableBmp.Width - 1 do
        for j = 0 to gotoTableBmp.Height - 1 do
            gotoTableBmp.SetPixel(i, j, colorOfGoto comp.GotoTable.[j].[i])
    gotoTableBmp.Save(path + "-gotoTable.bmp")