module FsLexYaccLiteRuntime

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

    member lexbuf.Lexeme = lexbuf.String.Substring(lexbuf.ScanStart, lexbuf.LexemeLength)

    member lexbuf.LexemeChar(i) =
        if i < 0 || i >= lexbuf.LexemeLength then raise (IndexOutOfRangeException())
        lexbuf.String.[lexbuf.ScanStart + i]

    member lexbuf.NewLine() =
        let pos = lexbuf.EndPos
        lexbuf.EndPos <- { pos with Line = pos.Line + 1; StartOfLine = pos.AbsoluteOffset }

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

type LexTables(asciiAlphabetTable : uint16[], nonAsciiCharRangeTable : uint16[], nonAsciiAlphabetTable : uint16[], transitionTable: int16[][], acceptTable: int16[]) =

    let [<Literal>] sentinel = -1

    let endScan lexbuf =
        if lexbuf.AcceptAction < 0 then
            failwith "unrecognized input"
        lexbuf.StartPos <- lexbuf.EndPos
        lexbuf.EndPos <- { lexbuf.EndPos with AbsoluteOffset = lexbuf.EndPos.AbsoluteOffset + lexbuf.LexemeLength }
        lexbuf.AcceptAction

    let rec scan lexbuf state =
        let a = int acceptTable.[state]
        if a <> sentinel then
            lexbuf.LexemeLength <- lexbuf.ScanLength
            lexbuf.AcceptAction <- a

        if lexbuf.ScanLength = lexbuf.String.Length - lexbuf.ScanStart then
            let snew = int transitionTable.[state].[transitionTable.[state].Length - 1] // get eof entry
            if snew = sentinel then
                endScan lexbuf
            elif not lexbuf.IsPastEndOfStream then
                lexbuf.IsPastEndOfStream <- true
                scan lexbuf snew
            else
                failwith "End of file on lexing stream"
        else
            let inp = lexbuf.String.[lexbuf.ScanStart + lexbuf.ScanLength]

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
                endScan lexbuf
            else
                lexbuf.ScanLength <- lexbuf.ScanLength + 1
                scan lexbuf snew

    member this.Interpret(lexBuffer : LexBuffer) =
        lexBuffer.ScanStart <- lexBuffer.ScanStart + lexBuffer.LexemeLength
        lexBuffer.ScanLength <- 0
        lexBuffer.LexemeLength <- 0
        lexBuffer.AcceptAction <- -1
        scan lexBuffer 0

/// The information accessible via the parseState value within parser actions.
type IParseState =
    /// Get the start and end position for the terminal or non-terminal at a given index matched by the production
    abstract InputRange: int -> Position * Position
    /// Get the full range of positions matched by the production
    abstract ResultRange: Position * Position
    /// Get the value produced by the terminal or non-terminal at the given position
    abstract GetInput: int -> obj
    /// Get the store of local values associated with this parser
    abstract ParserLocalStore : Dictionary<string, obj>

type ValueInfo =
    struct
        val value: obj
        val startPos: Position
        val endPos: Position
        new (value, startPos, endPos) = { value = value; startPos = startPos; endPos = endPos }
    end

exception EndOfTokenStream

type ParseTables<'tok>(reductions : int -> IParseState -> obj, endOfInputTag : int, tagOfToken : 'tok -> int, dataOfToken : 'tok -> obj, reductionSymbolCounts : uint16[], productionToNonTerminalTable : uint16[], maxProductionBodyLength : int, gotoTableBuckets : int16 [], gotoTableEntries : int16 [], nonTerminalsCount : int, actionTable_buckets : int16 [], actionTable_entries : int16 [], actionTable_defaultActions : int16 [], terminalsCount : int) =

    let lookup (buckets : int16 []) (entries : int16 []) (key : int) =
        let entryIndex = int buckets.[key % buckets.Length]
        if entryIndex >= 0 then
            let mutable pointer = 2 * entryIndex
            let mutable cont = true
            let mutable result = Int32.MinValue
            while cont do
                let head = entries.[pointer]
                if key = int (if head >= 0s then head else ~~~head) then
                    cont <- false
                    result <- int entries.[pointer + 1]
                elif head < 0s then
                    pointer <- pointer + 2
                else
                    cont <- false
            result
        else Int32.MinValue

    member this.Interpret(lexer : LexBuffer -> 'tok, lexbuf : LexBuffer, initialState : int) =
        let mutable cont = true
        let mutable haveLookahead = false
        let mutable lookahead = Unchecked.defaultof<'tok>
        let mutable lookaheadEndPos = Unchecked.defaultof<Position>
        let mutable lookaheadStartPos = Unchecked.defaultof<Position>
        let localStore = Dictionary<string, obj>()
        localStore.["LexBuffer"] <- lexbuf
        let stateStack = Stack<int>()
        stateStack.Push(initialState)
        let valueStack = Stack<ValueInfo>()

        let mutable lhsStartPos = Unchecked.defaultof<Position>
        let mutable lhsEndPos = Unchecked.defaultof<Position>
        let ruleStartPoss = Array.zeroCreate<Position> maxProductionBodyLength
        let ruleEndPoss   = Array.zeroCreate<Position> maxProductionBodyLength
        let ruleValues    = Array.zeroCreate<obj> maxProductionBodyLength

        let parseState =
            { new IParseState with
                member p.InputRange(n) = (ruleStartPoss.[n-1], ruleEndPoss.[n-1])
                member p.GetInput(n)    = ruleValues.[n-1]
                member p.ResultRange    = (lhsStartPos, lhsEndPos)
                member p.ParserLocalStore = localStore
            }
        
        while cont do

            let state = stateStack.Peek()

            if (not haveLookahead) && (not lexbuf.IsPastEndOfStream) then
                try
                    lookahead <- lexer lexbuf // exn
                    haveLookahead <- true
                    lookaheadStartPos <- lexbuf.StartPos
                    lookaheadEndPos <- lexbuf.EndPos
                with EndOfTokenStream -> ()

            let tag =
                if haveLookahead then
                    tagOfToken lookahead
                else
                    endOfInputTag

            let action =
                let v = lookup actionTable_buckets actionTable_entries (terminalsCount * state + tag)
                if v <> System.Int32.MinValue then
                    v
                else
                    int actionTable_defaultActions.[state]

            if action = -1 then
                cont <- false
            elif action = -2 then
                failwith "parse error"
            elif action >= 0 then
                let nextState = action
                let data = dataOfToken lookahead
                valueStack.Push(ValueInfo(data, lookaheadStartPos, lookaheadEndPos))
                stateStack.Push(nextState)
                haveLookahead <- false
            else
                let prod = - action - 3
                let n = int reductionSymbolCounts.[prod]
                lhsStartPos <- Position_Empty
                lhsEndPos <- Position_Empty
                for i = 0 to n - 1 do
                    let topVal = valueStack.Peek()
                    valueStack.Pop() |> ignore
                    stateStack.Pop() |> ignore
                    ruleValues.[(n-i)-1] <- topVal.value
                    ruleStartPoss.[(n-i)-1] <- topVal.startPos
                    ruleEndPoss.[(n-i)-1] <- topVal.endPos
                    if lhsEndPos.IsEmpty then lhsEndPos <- topVal.endPos
                    if not topVal.startPos.IsEmpty then lhsStartPos <- topVal.startPos
                let redResult = reductions prod parseState
                valueStack.Push(ValueInfo(redResult, lhsStartPos, lhsEndPos))
                let currState = stateStack.Peek()
                let newGotoState = lookup gotoTableBuckets gotoTableEntries (nonTerminalsCount * currState + int productionToNonTerminalTable.[prod])
                stateStack.Push(newGotoState)

        valueStack.Peek().value

