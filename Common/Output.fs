module Output

open System
open System.IO

let outputCode (os : TextWriter) (indent : int) (code : string) =
    
    let lines = code.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)

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