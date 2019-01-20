module FsLexYaccLite.Parsing

open System.Collections.Generic
open FsLexYaccLite.Lexing

/// The information accessible via the <c>parseState</c> value within parser actions.
type IParseState =
    /// Get the start and end position for the terminal or non-terminal at a given index matched by the production
    abstract InputRange: int -> Position * Position
    /// Get the full range of positions matched by the production
    abstract ResultRange: Position * Position
    /// Get the value produced by the terminal or non-terminal at the given position
    abstract GetInput: int -> obj 
    /// Get the store of local values associated with this parser
    abstract ParserLocalStore : IDictionary<string, obj>

type AssocTable(elemTab:uint16[], offsetTab:uint16[]) =
    let cache = Dictionary<int, int>()

    member t.readAssoc (minElemNum,maxElemNum,defaultValueOfAssoc,keyToFind) =     
        let elemNumber : int = (minElemNum+maxElemNum)/2
        if elemNumber = maxElemNum 
        then defaultValueOfAssoc
        else 
            let x = int elemTab.[elemNumber*2]
            if keyToFind = x then 
                int elemTab.[elemNumber*2+1]
            elif keyToFind < x then t.readAssoc (minElemNum ,elemNumber,defaultValueOfAssoc,keyToFind)
            else                    t.readAssoc (elemNumber+1,maxElemNum,defaultValueOfAssoc,keyToFind)

    member t.Read(rowNumber, keyToFind) =
        
        let mutable res = 0 
        let cacheKey = (rowNumber <<< 16) ||| keyToFind
        let ok = cache.TryGetValue(cacheKey, &res) 
        if ok then res 
        else
            let headOfTable = int offsetTab.[rowNumber]
            let firstElemNumber = headOfTable + 1           
            let numberOfElementsInAssoc = int elemTab.[headOfTable*2]
            let defaultValueOfAssoc = int elemTab.[headOfTable*2+1]          
            let res = t.readAssoc (firstElemNumber,(firstElemNumber+numberOfElementsInAssoc),defaultValueOfAssoc,keyToFind)
            cache.[cacheKey] <- res
            res

type ValueInfo = 
    struct
        val value: obj
        val startPos: Position
        val endPos: Position
        new (value, startPos, endPos) = { value = value; startPos = startPos; endPos = endPos }
    end

type Tables<'tok>(reductions : (IParseState -> obj) array, endOfInputTag : int, tagOfToken : 'tok -> int, dataOfToken : 'tok -> obj, reductionSymbolCounts : uint16[], productionToNonTerminalTable : uint16[], maxProductionBodyLength : int, gotoTableBuckets : int16 [], gotoTableEntries : int16 [], nonTerminalsCount : int, actionTable_buckets : int16 [], actionTable_entries : int16 [], actionTable_defaultActions : int16 [], terminalsCount : int) =
    
    let rec findEntry (entries : int16 []) key p =
        let x = entries.[2 * p]
        let hasNext = x < 0s
        let k = int (if 0s <= x then x else ~~~x)

        if key = k then
            int entries.[2 * p + 1]
        elif hasNext then
            findEntry entries key (p + 1)
        else System.Int32.MinValue
    
    let lookup (buckets : int16 []) (entries : int16 []) (key : int) =
        let bucket = int buckets.[key % buckets.Length]
        if bucket >= 0 then
            findEntry entries key bucket
        else System.Int32.MinValue

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
                member p.ParserLocalStore = localStore :> IDictionary<_,_>
            }       

        while cont do                                                                                    
            let state = stateStack.Peek()

            if (not haveLookahead) && (not lexbuf.IsPastEndOfStream) then
                haveLookahead <- true
                lookahead <- lexer lexbuf
                lookaheadStartPos <- lexbuf.StartPos
                lookaheadEndPos <- lexbuf.EndPos
                
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

            if action = int System.Int16.MaxValue then
                cont <- false
            elif action = int System.Int16.MinValue then
                failwith "parse error"
            elif action >= 0 then
                let nextState = action                             
                let data = dataOfToken lookahead
                valueStack.Push(ValueInfo(data, lookaheadStartPos, lookaheadEndPos))
                stateStack.Push(nextState)                                                           
                haveLookahead <- false
            else
                let prod = ~~~action                    
                let reduction = reductions.[prod]                                                             
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
                let redResult = reduction parseState                                                          
                valueStack.Push(ValueInfo(redResult, lhsStartPos, lhsEndPos))
                let currState = stateStack.Peek()
                let newGotoState = lookup gotoTableBuckets gotoTableEntries (nonTerminalsCount * currState + int productionToNonTerminalTable.[prod])
                stateStack.Push(newGotoState)
        
        valueStack.Peek().value

