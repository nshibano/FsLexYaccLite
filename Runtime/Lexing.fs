module FsLexYaccLite.Lexing 

open System
open System.Collections.Generic

/// Position information stored for lexing tokens
type Position = 
    { 
        /// The absolute offset of the column for the position
        AbsoluteOffset : int

        /// The line number in the input stream, assuming fresh positions have been updated 
        /// using AsNewLinePos() and by modifying the EndPos property of the LexBuffer.
        Line : int

        /// The absolute offset of the beginning of the line
        StartOfLine : int
    }

    /// Return the column number marked by the position, i.e. the difference between the AbsoluteOffset and the StartOfLineAbsoluteOffset
    member x.Column = x.AbsoluteOffset - x.StartOfLine
    /// Given a position just beyond the end of a line, return a position at the start of the next line
    member pos.NextLine = 
        { pos with 
                Line = pos.Line + 1 
                StartOfLine = pos.AbsoluteOffset }
    /// Given a position at the start of a token of length n, return a position just beyond the end of the token
    member pos.EndOfToken(n) = { pos with AbsoluteOffset = pos.AbsoluteOffset + n }
    /// Gives a position shifted by specified number of characters
    member pos.ShiftColumnBy(by) = {pos with AbsoluteOffset = pos.AbsoluteOffset + by }
    member pos.IsEmpty = pos.AbsoluteOffset = System.Int32.MinValue

/// Dummy position value which represents an absence of position information.
let Position_Empty = 
    { AbsoluteOffset = Int32.MinValue
      Line = 0
      StartOfLine = 0 }

/// Position corresponding to the first line (line number 1) in a given file
let Position_Zero =
    { AbsoluteOffset = 0
      Line = 1
      StartOfLine = 0 }

type LexBuffer =
    { String : string
      mutable ScanStart : int
      mutable ScanLength : int
      mutable LexemeLength : int
      mutable AcceptAction : int
      mutable IsPastEndOfStream : bool
      mutable StartPos : Position
      mutable EndPos : Position
      LocalStore : Dictionary<string, obj> }
    member x.BufferMaxScanLength = x.String.Length - x.ScanStart
    member x.BufferScanPos = x.ScanStart + x.ScanLength
    member x.Lexeme = x.String.Substring(x.ScanStart, x.LexemeLength)
    static member FromString (s:string) =
        { LexBuffer.String = s
          ScanStart = 0
          ScanLength = 0
          LexemeLength = 0   
          AcceptAction = -1
          IsPastEndOfStream = false
          StartPos = Position_Zero
          EndPos = Position_Zero
          LocalStore = Dictionary() }

type UnicodeTables(asciiAlphabetTable : uint16[], nonAsciiCharRangeTable : uint16[], nonAsciiAlphabetTable : uint16[], transitionTable: int16[][], acceptTable: int16[]) =
        
    let [<Literal>] sentinel = -1
        
    let endOfScan lexBuffer =
        if lexBuffer.AcceptAction < 0 then 
            failwith "unrecognized input"
        lexBuffer.StartPos <- lexBuffer.EndPos
        lexBuffer.EndPos <- lexBuffer.EndPos.EndOfToken(lexBuffer.LexemeLength)
        lexBuffer.AcceptAction
        
    let rec scanUntilSentinel lexBuffer state =
        let a = int acceptTable.[state] 
        if a <> sentinel then 
            lexBuffer.LexemeLength <- lexBuffer.ScanLength
            lexBuffer.AcceptAction <- a
            
        if lexBuffer.ScanLength = lexBuffer.BufferMaxScanLength then
            let snew = int transitionTable.[state].[transitionTable.[state].Length - 1] // get eof entry
            if snew = sentinel then 
                endOfScan lexBuffer
            elif not lexBuffer.IsPastEndOfStream then
                lexBuffer.IsPastEndOfStream <- true
                scanUntilSentinel lexBuffer snew
            else
                failwith "End of file on lexing stream"

        else
            let inp = lexBuffer.String.[lexBuffer.BufferScanPos]

            let alphabet =
                if inp < '\128' then
                    int asciiAlphabetTable.[int inp]
                else
                    let key = uint16 inp
                    let mutable i = 0
                    let mutable j = nonAsciiCharRangeTable.Length
                    while j - i > 1 do
                        let k = i + (j - i) / 2
                        if nonAsciiCharRangeTable.[k] <= key then
                            i <- k
                        else
                            j <- k
                    int nonAsciiAlphabetTable.[i]

            let snew = int transitionTable.[state].[alphabet]

            if snew = sentinel then 
                endOfScan lexBuffer
            else 
                lexBuffer.ScanLength <- lexBuffer.ScanLength + 1
                scanUntilSentinel lexBuffer snew

    member this.Interpret(lexBuffer : LexBuffer) = 
        lexBuffer.ScanStart <- lexBuffer.ScanStart + lexBuffer.LexemeLength
        lexBuffer.ScanLength <- 0
        lexBuffer.LexemeLength <- 0
        lexBuffer.AcceptAction <- -1
        scanUntilSentinel lexBuffer 0
