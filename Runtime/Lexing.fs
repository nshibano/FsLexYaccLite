// (c) Microsoft Corporation 2005-2009.
namespace Microsoft.FSharp.Text.Lexing 

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

        /// The line number for the position in the original source file
        OriginalLine : int

        /// The absolute offset of the beginning of the line
        StartOfLine : int

        /// The file name associated with the input stream.
        FileName : string
    }

    /// Return the column number marked by the position, i.e. the difference between the AbsoluteOffset and the StartOfLineAbsoluteOffset
    member x.Column = x.AbsoluteOffset - x.StartOfLine
    /// Given a position just beyond the end of a line, return a position at the start of the next line
    member pos.NextLine = 
        { pos with 
                OriginalLine = pos.OriginalLine + 1
                Line = pos.Line + 1 
                StartOfLine = pos.AbsoluteOffset }
    /// Given a position at the start of a token of length n, return a position just beyond the end of the token
    member pos.EndOfToken(n) = { pos with AbsoluteOffset = pos.AbsoluteOffset + n }
    /// Gives a position shifted by specified number of characters
    member pos.ShiftColumnBy(by) = {pos with AbsoluteOffset = pos.AbsoluteOffset + by }
    /// Get an arbitrary position, with the empty string as filename, and  
    static member Empty = 
        { FileName = ""
          Line = 0
          OriginalLine = 0
          StartOfLine = 0
          AbsoluteOffset = 0 }
    /// Get a position corresponding to the first line (line number 1) in a given file
    static member FirstLine(filename) = 
        { FileName = filename
          OriginalLine = 1
          Line = 1
          StartOfLine = 0
          AbsoluteOffset = 0 }
    
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
          StartPos = Position.Empty
          EndPos = Position.Empty
          LocalStore = Dictionary() }

type UnicodeTables(alphabetTable : uint16[], transitionTable: int16[][], acceptTable: int16[]) =
        
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
            let snew = int transitionTable.[state].[alphabetTable.Length] // get Eof entry
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
                let key = uint16 inp
                let mutable i = 0
                let mutable j = alphabetTable.Length
                while j - i > 1 do
                    let k = i + (j - i) / 2
                    if alphabetTable.[k] <= key then
                        i <- k
                    else
                        j <- k
                i

            let snew = int transitionTable.[state].[alphabet]

            if snew = sentinel then 
                endOfScan lexBuffer
            else 
                lexBuffer.ScanLength <- lexBuffer.ScanLength + 1
                scanUntilSentinel lexBuffer snew

    member this.Interpret(initialState, lexBuffer : LexBuffer) = 
        lexBuffer.ScanStart <- lexBuffer.ScanStart + lexBuffer.LexemeLength
        lexBuffer.ScanLength <- 0
        lexBuffer.LexemeLength <- 0
        lexBuffer.AcceptAction <- -1
        scanUntilSentinel lexBuffer initialState
