module FsLexYaccLite.Parsing

open System.Collections.Generic
open FsLexYaccLite.Lexing

/// The information accessible via the <c>parseState</c> value within parser actions.
type IParseState =
    /// Get the start and end position for the terminal or non-terminal at a given index matched by the production
    abstract InputRange: int -> Position * Position
    /// Get the end position for the terminal or non-terminal at a given index matched by the production
    abstract InputEndPosition: int -> Position 
    /// Get the start position for the terminal or non-terminal at a given index matched by the production
    abstract InputStartPosition: int -> Position 
    /// Get the full range of positions matched by the production
    abstract ResultRange: Position * Position
    /// Get the value produced by the terminal or non-terminal at the given position
    abstract GetInput: int -> obj 
    /// Get the store of local values associated with this parser
    abstract ParserLocalStore : IDictionary<string,obj>

type AssocTable(elemTab:uint16[], offsetTab:uint16[]) =
    let cache = Dictionary()

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

    member t.Read(rowNumber ,keyToFind) =
        
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

type Tables<'tok>(reductions : (IParseState -> obj) array, endOfInputTag : int, tagOfToken : 'tok -> int, dataOfToken : 'tok -> obj, actionTableElements : uint16[], actionTableRowOffsets : uint16[], reductionSymbolCounts : uint16[], gotos: uint16[], sparseGotoTableRowOffsets : uint16[], productionToNonTerminalTable : uint16[]) =
    let [<Literal>] shiftFlag = 0x0000
    let [<Literal>] reduceFlag = 0x4000
    let [<Literal>] errorFlag = 0x8000
    let [<Literal>] acceptFlag = 0xc000
    let [<Literal>] actionMask = 0xc000

    let actionValue action = action &&& (~~~ actionMask)                                    
    let actionKind action = action &&& actionMask

    member this.Interpret(lexer : LexBuffer -> 'tok, lexbuf : LexBuffer, initialState : int) =                                                                      
        let localStore = new Dictionary<string,obj>() in
        localStore.["LexBuffer"] <- lexbuf
        let stateStack : Stack<int> = new Stack<_>()
        stateStack.Push(initialState);
        let valueStack = new Stack<ValueInfo>()
        let mutable haveLookahead = false                                                                              
        let mutable lookaheadToken = Unchecked.defaultof<'tok>
        let mutable lookaheadEndPos = Unchecked.defaultof<Position>
        let mutable lookaheadStartPos = Unchecked.defaultof<Position>
        let mutable finished = false

        let ruleStartPoss = (Array.zeroCreate 100 : Position array)              
        let ruleEndPoss   = (Array.zeroCreate 100 : Position array)              
        let ruleValues    = (Array.zeroCreate 100 : obj array)              
        let lhsPos        = (Array.zeroCreate 2 : Position array)                                            
        let reductions = reductions
        let actionTable = AssocTable(actionTableElements, actionTableRowOffsets)
        let gotoTable = AssocTable(gotos, sparseGotoTableRowOffsets)

        let parseState =                                                                                            
            { new IParseState with 
                member p.InputRange(n) = ruleStartPoss.[n-1], ruleEndPoss.[n-1]; 
                member p.InputStartPosition(n) = ruleStartPoss.[n-1]
                member p.InputEndPosition(n) = ruleEndPoss.[n-1]; 
                member p.GetInput(n)    = ruleValues.[n-1];        
                member p.ResultRange    = (lhsPos.[0], lhsPos.[1]);  
                member p.ParserLocalStore = (localStore :> IDictionary<_,_>); 
            }       

        while not finished do                                                                                    
            if stateStack.Count = 0 then 
                finished <- true
            else
                let state = stateStack.Peek()
                let action = 
                    if not haveLookahead then 
                        if lexbuf.IsPastEndOfStream then 
                            haveLookahead <- false
                        else 
                            lookaheadToken <- lexer lexbuf
                            lookaheadStartPos <- lexbuf.StartPos
                            lookaheadEndPos <- lexbuf.EndPos
                            haveLookahead <- true
                    let tag = 
                        if haveLookahead then tagOfToken lookaheadToken 
                        else endOfInputTag   
                                    
                    actionTable.Read(state,tag)
                        
                let kind = actionKind action 
                if kind = shiftFlag then
                    let nextState = actionValue action                                     
                    if not haveLookahead then failwith "unreachable"
                    let data = dataOfToken lookaheadToken
                    valueStack.Push(ValueInfo(data, lookaheadStartPos, lookaheadEndPos));
                    stateStack.Push(nextState);                                                                
                    haveLookahead <- false

                elif kind = reduceFlag then
                    let prod = actionValue action                                     
                    let reduction = reductions.[prod]                                                             
                    let n = int reductionSymbolCounts.[prod]
                    lhsPos.[0] <- Position_Empty                                                                     
                    lhsPos.[1] <- Position_Empty
                    for i = 0 to n - 1 do                                                                             
                        if valueStack.Count = 0 then failwith "unreachable"
                        let topVal = valueStack.Peek()
                        valueStack.Pop() |> ignore
                        stateStack.Pop() |> ignore
                        ruleValues.[(n-i)-1] <- topVal.value;  
                        ruleStartPoss.[(n-i)-1] <- topVal.startPos;  
                        ruleEndPoss.[(n-i)-1] <- topVal.endPos;  
                        if lhsPos.[1].IsEmpty then lhsPos.[1] <- topVal.endPos;
                        if not topVal.startPos.IsEmpty then lhsPos.[0] <- topVal.startPos
                    let redResult = reduction parseState                                                          
                    valueStack.Push(ValueInfo(redResult, lhsPos.[0], lhsPos.[1]));
                    let currState = stateStack.Peek()
                    let newGotoState = gotoTable.Read(int productionToNonTerminalTable.[prod], currState)
                    stateStack.Push(newGotoState)
                elif kind = errorFlag then
                    failwith "parse error"
                elif kind = acceptFlag then 
                    finished <- true
                else
                    failwith "unreachable"

        valueStack.Peek().value

