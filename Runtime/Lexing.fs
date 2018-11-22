// (c) Microsoft Corporation 2005-2009.

namespace Microsoft.FSharp.Text.Lexing 

    open System.Collections.Generic

    /// Position information stored for lexing tokens
    type Position = 
        { 
          /// The file name for the position
          pos_fname : string
          /// The line number for the position
          pos_lnum : int;
          /// The line number for the position in the original source file
          pos_orig_lnum : int;
          /// The absolute offset of the beginning of the line
          pos_bol : int
          /// The absolute offset of the column for the position
          pos_cnum : int; }
        /// The file name associated with the input stream.
        member x.FileName = x.pos_fname
        /// The line number in the input stream, assuming fresh positions have been updated 
        /// using AsNewLinePos() and by modifying the EndPos property of the LexBuffer.
        member x.Line = x.pos_lnum
        member x.OriginalLine = x.pos_orig_lnum
        member x.Char = x.pos_cnum
        /// The character number in the input stream
        member x.AbsoluteOffset = x.pos_cnum
        member x.StartOfLine = x.pos_bol
        /// Return absolute offset of the start of the line marked by the position
        member x.StartOfLineAbsoluteOffset = x.pos_bol
        /// Return the column number marked by the position, i.e. the difference between the AbsoluteOffset and the StartOfLineAbsoluteOffset
        member x.Column = x.pos_cnum - x.pos_bol
        /// Given a position just beyond the end of a line, return a position at the start of the next line
        member pos.NextLine = 
            { pos with 
                    pos_orig_lnum = pos.OriginalLine + 1;
                    pos_lnum = pos.Line+1; 
                    pos_bol = pos.AbsoluteOffset }
        /// Given a position at the start of a token of length n, return a position just beyond the end of the token
        member pos.EndOfToken(n) = {pos with pos_cnum=pos.pos_cnum + n }
        member pos.AsNewLinePos() = pos.NextLine
        /// Gives a position shifted by specified number of characters
        member pos.ShiftColumnBy(by) = {pos with pos_cnum = pos.pos_cnum + by}
        /// Get an arbitrary position, with the empty string as filename, and  
        static member Empty = 
            { pos_fname=""; 
              pos_lnum= 0; 
              pos_orig_lnum = 0;
              pos_bol= 0; 
              pos_cnum=0 }
        /// Get a position corresponding to the first line (line number 1) in a given file
        static member FirstLine(filename) = 
            { pos_fname=filename; 
              pos_orig_lnum = 1;
              pos_lnum= 1; 
              pos_bol= 0; 
              pos_cnum=0 }
    
    type LexBuffer =
        { String : string
          mutable BufferScanStart : int
          mutable BufferScanLength : int
          mutable LexemeLength : int
          mutable BufferAcceptAction : int
          mutable IsPastEndOfStream : bool
          mutable StartPos : Position
          mutable EndPos : Position
          BufferLocalStore : Dictionary<string, obj> }
        member x.BufferMaxScanLength = x.String.Length - x.BufferScanStart
        member x.BufferScanPos = x.BufferScanStart + x.BufferScanLength
        member x.EndOfScan() : int =
            if x.BufferAcceptAction < 0 then 
                failwith "unrecognized input"

            //  Printf.printf "endOfScan %d state %d on unconsumed input '%c' (%d)\n" a s (Char.chr inp) inp;
            //   Printf.eprintf "accept, lexeme = %s\n" (lexeme lexBuffer); 
            x.StartPos <- x.EndPos;
            x.EndPos <- x.EndPos.EndOfToken(x.LexemeLength);
            x.BufferAcceptAction
        member x.Lexeme = x.String.Substring(x.BufferScanStart, x.LexemeLength)
        static member FromString (s:string) =
            { LexBuffer.String = s
              BufferScanStart = 0
              BufferScanLength = 0
              LexemeLength = 0   
              BufferAcceptAction = -1
              IsPastEndOfStream = false
              StartPos = Position.Empty
              EndPos = Position.Empty
              BufferLocalStore = Dictionary() }

    type UnicodeTables(trans: uint16[] array, accept: uint16[]) = 
        let startInterpret(lexBuffer : LexBuffer) = 
            lexBuffer.BufferScanStart <- lexBuffer.BufferScanStart + lexBuffer.LexemeLength
            lexBuffer.BufferScanLength <- 0;
            lexBuffer.LexemeLength <- 0;
            lexBuffer.BufferAcceptAction <- -1;

        let afterRefill (trans: uint16[] array,sentinel,lexBuffer:LexBuffer,scanUntilSentinel,endOfScan,state,eofPos) = 
            // end of file occurs if we couldn't extend the buffer 
            if lexBuffer.BufferScanLength = lexBuffer.BufferMaxScanLength then  
                let snew = int trans.[state].[eofPos] // == EOF 
                if snew = sentinel then 
                    endOfScan()
                else 
                    if lexBuffer.IsPastEndOfStream then failwith "End of file on lexing stream";
                    lexBuffer.IsPastEndOfStream <- true;
                    // Printf.printf "state %d --> %d on eof\n" state snew;
                    scanUntilSentinel(lexBuffer,snew)
            else 
                scanUntilSentinel(lexBuffer, state)

        let onAccept (lexBuffer:LexBuffer,a) = 
            lexBuffer.LexemeLength <- lexBuffer.BufferScanLength;
            lexBuffer.BufferAcceptAction <- a
        
        let sentinel = 255 * 256 + 255 
        let numUnicodeCategories = 30 
        let numLowUnicodeChars = 128 
        let numSpecificUnicodeChars = (trans.[0].Length - 1 - numLowUnicodeChars - numUnicodeCategories)/2
        let lookupUnicodeCharacters (state,inp) = 
            let inpAsInt = int inp
            // Is it a fast ASCII character?
            if inpAsInt < numLowUnicodeChars then 
                int trans.[state].[inpAsInt]
            else 
                // Search for a specific unicode character
                let baseForSpecificUnicodeChars = numLowUnicodeChars
                let rec loop i = 
                    if i >= numSpecificUnicodeChars then 
                        // OK, if we failed then read the 'others' entry in the alphabet,
                        // which covers all Unicode characters not covered in other
                        // ways
                        let baseForUnicodeCategories = numLowUnicodeChars+numSpecificUnicodeChars*2
                        let unicodeCategory = System.Char.GetUnicodeCategory(inp)
                        //System.Console.WriteLine("inp = {0}, unicodeCategory = {1}", [| box inp; box unicodeCategory |]);
                        int trans.[state].[baseForUnicodeCategories + int32 unicodeCategory]
                    else 
                        // This is the specific unicode character
                        let c = char (int trans.[state].[baseForSpecificUnicodeChars+i*2])
                        //System.Console.WriteLine("c = {0}, inp = {1}, i = {2}", [| box c; box inp; box i |]);
                        // OK, have we found the entry for a specific unicode character?
                        if c = inp
                        then int trans.[state].[baseForSpecificUnicodeChars+i*2+1]
                        else loop(i+1)
                
                loop 0
        let eofPos    = numLowUnicodeChars + 2*numSpecificUnicodeChars + numUnicodeCategories 
        
        let rec scanUntilSentinel(lexBuffer,state) =
            // Return an endOfScan after consuming the input 
            let a = int accept.[state] 
            if a <> sentinel then 
                onAccept(lexBuffer,a)
            
            if lexBuffer.BufferScanLength = lexBuffer.BufferMaxScanLength then 
                //lexBuffer.DiscardInput();
              // end of file occurs if we couldn't extend the buffer 
                afterRefill (trans,sentinel,lexBuffer,scanUntilSentinel,lexBuffer.EndOfScan,state,eofPos)
            else
                // read a character - end the scan if there are no further transitions 
                let inp = lexBuffer.String.[lexBuffer.BufferScanPos]
                
                // Find the new state
                let snew = lookupUnicodeCharacters (state,inp)

                if snew = sentinel then 
                    lexBuffer.EndOfScan()
                else 
                    lexBuffer.BufferScanLength <- lexBuffer.BufferScanLength + 1;
                    // Printf.printf "state %d --> %d on '%c' (%d)\n" s snew (char inp) inp;
                    scanUntilSentinel(lexBuffer,snew)

        /// Interpret tables for a unicode lexer generated by fslex. 
        // Each row for the Unicode table has format 
        //      128 entries for ASCII characters
        //      A variable number of 2*UInt16 entries for SpecificUnicodeChars 
        //      30 entries, one for each UnicodeCategory
        //      1 entry for EOF
        member tables.Interpret(initialState,lexBuffer : LexBuffer) = 
            startInterpret(lexBuffer)
            scanUntilSentinel(lexBuffer, initialState)

        static member Create(trans,accept) = new UnicodeTables(trans,accept)
