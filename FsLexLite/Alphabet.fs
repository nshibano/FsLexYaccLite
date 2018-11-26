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
      IndexTable : int array
      /// Map for translation of regex.
      AlphabetsOfCharset : Map<Set<char * char>, Set<int>> }

    member table.AlphabetOther = table.AlphabetCount - 1
    member table.AlphabetEof = table.AlphabetCount
    member table.AlphabetOfChar c =
        let index = table.IndexTable.[binchopFloor table.RangeTable (int c)]
        if index >= 0 then
            index
        else
            table.AlphabetOther

let createTable (spec : Spec) =

    let rangeTable = List<int>([| 0 |])
    let charSetsTable = List<Set<Set<char * char>>>([| Set.empty |])

    /// add split between (value - 1) and value.
    let addSplit (value : int) =
        let i = binchopFloor rangeTable (value - 1)
        let j = binchopFloor rangeTable value
        if i = j then
            rangeTable.Insert(i + 1, value)
            charSetsTable.Insert(i + 1, charSetsTable.[i])
 
    let addCharRangeSplit (charset : Set<char * char>) (cFirst: char) (cLast : char) =
        if cFirst > cLast then failwithf "invalid char range specifier: \\u%04x to \\u%04x" (int cFirst) (int cLast)
        addSplit (int cFirst)
        addSplit (int cLast + 1)
        for i = binchopFloor rangeTable (int cFirst) to binchopFloor rangeTable (int cLast) do
            charSetsTable.[i] <- Set.add charset charSetsTable.[i]
    
    let rec regexpLoop (regexp : Regexp) =
        match regexp with
        | Inp inp ->
            match inp with
            | CharSet set
            | NotCharSet set -> Set.iter (fun (first, last) -> addCharRangeSplit set first last) set
            | Any
            | Eof -> ()
            | Alphabet _ -> failwith "dontcare"
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
    let nonOtherAlphabetCount = Set.count (Set charSetsTable) - 1
    let indexTable = List<int>()
    let mutable alphabetOfCharSets = Map<Set<Set<char * char>>, int>([| (Set.empty, nonOtherAlphabetCount) |]) // the empty corresponds to the alphabet 'other' 

    for i = 0 to rangeTable.Count - 1 do
        match Map.tryFind charSetsTable.[i] alphabetOfCharSets with
        | Some alphabet -> indexTable.Add(alphabet)
        | None ->
            let newAlphabet = alphabetOfCharSets.Count
            alphabetOfCharSets <- Map.add charSetsTable.[i] newAlphabet alphabetOfCharSets
            indexTable.Add(newAlphabet)
    
    if alphabetOfCharSets.Count - 1 <> nonOtherAlphabetCount then failwith "dontcare"

    let mutable alphabetsOfCharSet = Map.empty<Set<char * char>, Set<int>>
    Map.iter (fun charsets alphabet ->
        Set.iter (fun charset ->
            let alphabets =
                match alphabetsOfCharSet.TryFind(charset) with
                | Some alphabets -> alphabets
                | None -> Set.empty
            alphabetsOfCharSet <- Map.add charset (Set.add alphabet alphabets) alphabetsOfCharSet) charsets) alphabetOfCharSets

    { AlphabetCount = nonOtherAlphabetCount + 1
      RangeTable = rangeTable.ToArray()
      IndexTable = indexTable.ToArray()
      AlphabetsOfCharset = alphabetsOfCharSet }

let translate (table : AlphabetTable) (spec : Spec) =

    let AlphabetsOfCharSet (charset : Set<char * char>) =
        table.AlphabetsOfCharset.[charset]

    let regexOfAlphabets alphabets =
        let ary = Array.ofSeq alphabets
        Array.sortInPlace ary
        Alt (List.ofArray (Array.map (fun a -> Inp (Alphabet a)) ary))
    
    let rec regexpMap (regexp : Regexp) =
        match regexp with
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
        | Inp (Alphabet _) -> failwith "dontcare"

    { spec with
        Macros = List.map (fun (name, re) -> (name, regexpMap re)) spec.Macros
        Rules = List.map (fun (name, args, clauses) -> (name, args, List.map (fun (regexp, code) -> (regexpMap regexp, code)) clauses)) spec.Rules }

