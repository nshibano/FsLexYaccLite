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

type UnicodeTables(alphabetTable : int16[], trans: int16[][], accept: int16[]) = 
        
    let [<Literal>] sentinel = -1

    let binchopFloor (table : int16[]) (key : int16) =
        if table.Length = 0 || key < table.[0] then -1
        else
            let mutable i = 0
            let mutable j = table.Length
            while j - i > 1 do
                let k = i + (j - i) / 2
                if table.[k] <= key then
                    i <- k
                else
                    j <- k
            i

    let alphabetOfChar (table : int16[]) (c : char) = binchopFloor alphabetTable (int16 c)
    let alphabetEof = alphabetTable.Length

    //let numUnicodeCategories = 30 
    //let numLowUnicodeChars = 128 
    //let numSpecificUnicodeChars = (trans.[0].Length - 1 - numLowUnicodeChars - numUnicodeCategories) / 2
    //let lookupUnicodeCharacters state inp = 
    //    let inpAsInt = int inp
    //    // Is it a fast ASCII character?
    //    if inpAsInt < numLowUnicodeChars then 
    //        int trans.[state].[inpAsInt]
    //    else 
    //        // Search for a specific unicode character
    //        let baseForSpecificUnicodeChars = numLowUnicodeChars
    //        let rec loop i = 
    //            if i >= numSpecificUnicodeChars then 
    //                // OK, if we failed then read the 'others' entry in the alphabet,
    //                // which covers all Unicode characters not covered in other
    //                // ways
    //                let baseForUnicodeCategories = numLowUnicodeChars+numSpecificUnicodeChars*2
    //                let unicodeCategory = Char.GetUnicodeCategory(inp)
    //                //System.Console.WriteLine("inp = {0}, unicodeCategory = {1}", [| box inp; box unicodeCategory |]);
    //                int trans.[state].[baseForUnicodeCategories + int32 unicodeCategory]
    //            else 
    //                // This is the specific unicode character
    //                let c = char (int trans.[state].[baseForSpecificUnicodeChars+i*2])
    //                //System.Console.WriteLine("c = {0}, inp = {1}, i = {2}", [| box c; box inp; box i |]);
    //                // OK, have we found the entry for a specific unicode character?
    //                if c = inp
    //                then int trans.[state].[baseForSpecificUnicodeChars+i*2+1]
    //                else loop(i+1)
                
    //        loop 0
    //let eofPos = numLowUnicodeChars + 2*numSpecificUnicodeChars + numUnicodeCategories 
        
    let endOfScan lexBuffer =
        if lexBuffer.AcceptAction < 0 then 
            failwith "unrecognized input"
        lexBuffer.StartPos <- lexBuffer.EndPos
        lexBuffer.EndPos <- lexBuffer.EndPos.EndOfToken(lexBuffer.LexemeLength)
        lexBuffer.AcceptAction
        
    let rec scanUntilSentinel lexBuffer state =
        // Return an endOfScan after consuming the input 
        let a = int accept.[state] 
        if a <> sentinel then 
            lexBuffer.LexemeLength <- lexBuffer.ScanLength
            lexBuffer.AcceptAction <- a
            
        if lexBuffer.ScanLength = lexBuffer.BufferMaxScanLength then 
            let snew = int trans.[state].[alphabetEof] // == EOF 
            if snew = sentinel then 
                endOfScan lexBuffer
            else 
                if lexBuffer.IsPastEndOfStream then failwith "End of file on lexing stream"
                lexBuffer.IsPastEndOfStream <- true
                // Printf.printf "state %d --> %d on eof\n" state snew;
                scanUntilSentinel lexBuffer snew

        else
            // read a character - end the scan if there are no further transitions 
            let inp = lexBuffer.String.[lexBuffer.BufferScanPos]
            let alphabet = alphabetOfChar alphabetTable inp
            // Find the new state
            let snew = int trans.[state].[alphabet] //alphabetOfChar alphabetTable inp //lookupUnicodeCharacters state inp

            if snew = sentinel then 
                endOfScan lexBuffer
            else 
                lexBuffer.ScanLength <- lexBuffer.ScanLength + 1
                // Printf.printf "state %d --> %d on '%c' (%d)\n" s snew (char inp) inp;
                scanUntilSentinel lexBuffer snew

    /// Interpret tables for a unicode lexer generated by fslex. 
    // Each row for the Unicode table has format 
    //      128 entries for ASCII characters
    //      A variable number of 2*UInt16 entries for SpecificUnicodeChars 
    //      30 entries, one for each UnicodeCategory
    //      1 entry for EOF
    member tables.Interpret(initialState, lexBuffer : LexBuffer) = 
        lexBuffer.ScanStart <- lexBuffer.ScanStart + lexBuffer.LexemeLength
        lexBuffer.ScanLength <- 0
        lexBuffer.LexemeLength <- 0
        lexBuffer.AcceptAction <- -1
        scanUntilSentinel lexBuffer initialState

    //static member Create(trans,accept) = new UnicodeTables(alphabetTable trans, accept)
