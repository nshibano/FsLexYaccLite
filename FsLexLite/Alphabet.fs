module FsLexYaccLite.Lex.Alphabet

open System.Collections.Generic
open FsLexYaccLite.Lex.Syntax

/// Returns index of the highest item of the items which is equal to or lower than the key.
/// If there is no such item returns -1.
/// Therefore, -1 <= returnValue <= table.Length - 1.
/// The table must be sorted in ascending order.
let binchopFloor (table : List<int>) (key : int) =
    if table.Count = 0 || key < table.[0] then -1
    else
        let mutable i = 0
        let mutable j = table.Count
        while j - i > 1 do
            let k = i + (j - i) / 2
            if table.[k] <= key then
                i <- k
            else
                j <- k
        i

/// add split between (value - 1) and value.
let addSplit (table : List<int>) (value : int) =
    let i = binchopFloor table (value - 1)
    let j = binchopFloor table value
    if i = j then
        table.Insert(i + 1, value)
 
let addSingleCharSplit (table : List<int>) (c : char) =
    addSplit table (int c)
    addSplit table (int c + 1)

let addCharRangeSplit (table : List<int>) (cFirst: char) (cLast : char) =
    if not (cFirst < cLast) then failwith "error"
    addSplit table (int cFirst)
    addSplit table (int cLast + 1)

let createTable (spec : Spec) =
    let table = List([| 0 |])

    let charSetItemLoop (item : CharSetItem) =
        match item with
        | SingleChar c -> addSingleCharSplit table c
        | CharRange (first, last) -> addCharRangeSplit table first last

    let inputLoop (input : Input) =
        match input with
        | CharSet l
        | NotCharSet l -> List.iter charSetItemLoop l
        | Any
        | Eof -> ()
        | Alphabet _ -> failwith "dontcare"
    
    let rec regexpLoop (regexp : Regexp) =
        match regexp with
        | Inp inp -> inputLoop inp
        | Alt l -> List.iter regexpLoop l
        | Seq l ->  List.iter regexpLoop l
        | Star regexp -> regexpLoop regexp
        | Macro _ -> ()
    
    for _, regexp in spec.Macros do
        regexpLoop regexp
    for _, _, clauses in spec.Rules do
        for regexp, _ in clauses do
            regexpLoop regexp

    table

/// Excluding Eof
let alphabetsCount (table : List<int>) = table.Count
let alphabetEof (table : List<int>) = table.Count
let alphabetOfChar (table : List<int>) (c : char) = binchopFloor table (int c)

let translate (table : List<int>) (spec : Spec) =

    let AlphabetsOfCharSet l =
        let accu = HashSet<int>()
        for item in l do
            match item with
            | SingleChar c -> accu.Add(alphabetOfChar table c) |> ignore
            | CharRange (first, last) ->
                if not (first < last) then failwith "dontcare"
                let first = alphabetOfChar table first
                let last = alphabetOfChar table last
                for x = first to last do
                    accu.Add(x) |> ignore
        accu

    let regexOfAlphabets set =
        let ary = Array.ofSeq set
        Array.sortInPlace ary
        Alt (List.ofArray (Array.map (fun a -> Inp (Alphabet a)) ary))
    
    let rec regexpMap (regexp : Regexp) =
        match regexp with
        | Inp (CharSet [SingleChar c]) -> Inp (Alphabet (alphabetOfChar table c))
        | Inp (CharSet l) -> regexOfAlphabets (AlphabetsOfCharSet l)
        | Inp (NotCharSet l) ->
            let set = AlphabetsOfCharSet l
            let accu = HashSet<int>()
            for i = 0 to (alphabetsCount table) - 1 do
                if not (set.Contains(i)) then accu.Add(i) |> ignore
            regexOfAlphabets accu
        | Inp Any -> regexOfAlphabets (Array.init (alphabetsCount table) (fun i -> i))
        | Inp Eof -> Inp (Alphabet (alphabetEof table))
        | Alt l -> Alt (List.map regexpMap l)
        | Seq l ->  Seq (List.map regexpMap l)
        | Star regexp -> Star (regexpMap regexp)
        | Macro _ -> regexp
        | Inp (CharSet [])
        | Inp (Alphabet _) -> failwith "dontcare"

    { spec with
        Macros = List.map (fun (name, re) -> (name, regexpMap re)) spec.Macros
        Rules = List.map (fun (name, args, clauses) -> (name, args, List.map (fun (regexp, code) -> (regexpMap regexp, code)) clauses)) spec.Rules }

