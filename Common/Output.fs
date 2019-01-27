module Output

open System
open System.IO

let outputCode (os : TextWriter) (indent : int) (code : string) =
    
    let lines = code.Split([| "\r\n"; "\n" |], StringSplitOptions.None)
    
    let getIndentLevel (s : string) =
        let mutable level = 0
        let mutable pos = 0
        while pos < s.Length do
            match s.[pos] with
            | ' ' ->
                level <- level + 1
                pos <- pos + 1
            | '\t' ->
                level <- level + 4
                pos <- pos + 1
            | _ -> pos <- s.Length
        level

    let shiftIndentLevel incr (s : string) =
        let current = getIndentLevel s
        String(' ', current + incr) + s.TrimStart([|' '; '\t'|])

    let setIndent (level : int) (lines : string array) =
        let min = Array.min (Array.map getIndentLevel lines)
        Array.map (shiftIndentLevel (level - min)) lines

    for line in setIndent indent lines do
        fprintfn os "%s" line

let outputUInt16Array (os : TextWriter) (name : string) (ary : int array) =
    fprintf os "let %s = [|" name
    for i = 0 to ary.Length - 1 do  
        if i <> 0 then
            fprintf os "; "
        fprintf os "%dus" ary.[i]
    fprintfn os "|]"

let outputInt16Array (os : TextWriter) (name : string) (ary : int array) =
    if ary.Length <= 16 then
        fprintf os "let %s = [| " name
        for i = 0 to ary.Length - 1 do  
            if i <> 0 then
                fprintf os "; "
            fprintf os "%ds" ary.[i]
        fprintfn os " |]"
    else
        fprintfn os "let %s =" name
        let n = ary.Length / 16
        for i = 0 to n do
            if i = 0 then
                fprintf os "    [| "
            else
                fprintf os "       "
        
            for j = 0 to (if i = n then ary.Length % 16 else 16) - 1 do
                if j <> 0 then
                    fprintf os "; "
                fprintf os "%ds" ary.[16 * i + j]
            if i < n then
                fprintfn os ""
            else
                fprintfn os "|]"
