module FsLexYaccLite.Lex.Macros

open System.Collections.Generic

open Syntax

let expand (macros : (Ident * Regexp) list) (regexps : Regexp list) =

    let dict = Dictionary()

    let rec regexpMap (re : Regexp) =
        match re with
        | Alt l -> Alt (List.map regexpMap l)
        | Seq l ->  Seq (List.map regexpMap l)
        | Star re -> Star (regexpMap re)
        | Macro name ->
            match dict.TryGetValue(name) with
            | true, re -> re
            | false, _ -> failwithf "macro %s is not defined" name
        | _ -> re

    for name, re in macros do
        dict.Add(name, regexpMap re)

    List.map regexpMap regexps 
