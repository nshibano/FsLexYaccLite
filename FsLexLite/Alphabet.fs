module FsLexYaccLite.Lex.Alphabet

open System.Collections.Generic
open FsLexYaccLite.Lex.Syntax

/// Returns index of the highest item of the items which is equal to or lower than the key.
/// If there is no such item returns -1.
/// Therefore, -1 <= returnValue <= table.Length - 1.
/// The table must be sorted in ascending order.
let binchopFloor (table : IReadOnlyList<int>) (key : int) =
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

type AlphabetTable =
    { 
      /// Including char range alphabets and the 'Other' alphabet. But excluding the 'Eof' alphabet.
      AlphabetCount : int
      /// The table which represents the char range of alphabets. To find table offset of the alphabet, do the binchpFloor with the char.
      RangeTable : int array
      /// IndexTable.[ofs] is alphabet index of the alphabet corresponds to char range starts from RangeTable.[ofs].
      IndexTable : int array }

    member table.AlphabetOther = table.AlphabetCount - 1
    member table.AlphabetEof = table.AlphabetCount

let alphabetOfChar (table : AlphabetTable) (c : char) =
    let index = table.IndexTable.[binchopFloor table.RangeTable (int c)]
    if index >= 0 then
        index
    else
        table.AlphabetOther

let createTable (spec : Spec) =

    let rangeTable = List([| 0 |])
    let nonOtherFlagTable = List([| false |])

    /// add split between (value - 1) and value.
    let addSplit (value : int) =
        let i = binchopFloor rangeTable (value - 1)
        let j = binchopFloor rangeTable value
        if i = j then
            rangeTable.Insert(i + 1, value)
            nonOtherFlagTable.Insert(i + 1, nonOtherFlagTable.[i])
 
    let addSingleCharSplit (c : char) =
        addSplit (int c)
        addSplit (int c + 1)
        nonOtherFlagTable.[binchopFloor rangeTable (int c)] <- true

    let addCharRangeSplit (cFirst: char) (cLast : char) =
        if not (cFirst < cLast) then failwithf "invalid char range specifier: \\u%04x to \\u%04x" (int cFirst) (int cLast)
        addSplit (int cFirst)
        addSplit (int cLast + 1)
        for i = binchopFloor rangeTable (int cFirst) to binchopFloor rangeTable (int cLast) do
            nonOtherFlagTable.[i] <- true

    let charSetItemLoop (item : CharSetItem) =
        match item with
        | SingleChar c -> addSingleCharSplit c
        | CharRange (first, last) -> addCharRangeSplit first last

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

    // assign alphabet indexes for charranges    
    let nonOtherCharRangeCount = Seq.length (Seq.filter id nonOtherFlagTable)
    let indexTable = List<int>()
    let mutable top = 0
    for i = 0 to rangeTable.Count - 1 do
        if nonOtherFlagTable.[i] then
            indexTable.Add(top)
            top <- top + 1
        else
            indexTable.Add(nonOtherCharRangeCount) // 'other' alphabet

    { AlphabetCount = nonOtherCharRangeCount + 1
      RangeTable = rangeTable.ToArray()
      IndexTable = indexTable.ToArray() }

let translate (table : AlphabetTable) (spec : Spec) =

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
            for i = 0 to table.AlphabetCount - 1 do
                if not (set.Contains(i)) then accu.Add(i) |> ignore
            regexOfAlphabets accu
        | Inp Any -> regexOfAlphabets (Array.init (table.AlphabetCount) (fun i -> i))
        | Inp Eof -> Inp (Alphabet table.AlphabetEof)
        | Alt l -> Alt (List.map regexpMap l)
        | Seq l ->  Seq (List.map regexpMap l)
        | Star regexp -> Star (regexpMap regexp)
        | Macro _ -> regexp
        | Inp (CharSet [])
        | Inp (Alphabet _) -> failwith "dontcare"

    { spec with
        Macros = List.map (fun (name, re) -> (name, regexpMap re)) spec.Macros
        Rules = List.map (fun (name, args, clauses) -> (name, args, List.map (fun (regexp, code) -> (regexpMap regexp, code)) clauses)) spec.Rules }

