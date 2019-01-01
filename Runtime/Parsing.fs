// (c) Microsoft Corporation 2005-2009. 

namespace Microsoft.FSharp.Text.Parsing
open Microsoft.FSharp.Text.Lexing

open System
open System.Collections.Generic

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
    // Dynamically typed, non-lexically scoped local store
    abstract ParserLocalStore : IDictionary<string,obj>
    /// Raise an error in this parse context
    abstract RaiseError<'b> : unit -> 'b

//-------------------------------------------------------------------------
// This context is passed to the error reporter when a syntax error occurs

/// The context provided when a parse error occurs
type ParseErrorContext<'tok>
         (//lexbuf: LexBuffer<_>,
          stateStack:int list,
          parseState: IParseState, 
          reduceTokens: int list, 
          currentToken: 'tok option, 
          reducibleProductions: int list list, 
          shiftableTokens: int list , 
          message : string) =
      //member x.LexBuffer = lexbuf
      /// The stack of state indexes active at the parse error 
      member x.StateStack  = stateStack
      /// The tokens that would cause a reduction at the parse error 
      member x.ReduceTokens = reduceTokens
      /// The token that caused the parse error
      member x.CurrentToken = currentToken
      /// The state active at the parse error 
      member x.ParseState = parseState
      /// The stack of productions that would be reduced at the parse error 
      member x.ReducibleProductions = reducibleProductions
      /// The token that would cause a shift at the parse error
      member x.ShiftTokens = shiftableTokens
      /// The message associated with the parse error
      member x.Message = message


//-------------------------------------------------------------------------
// This is the data structure emitted as code by FSYACC.  


type Tables<'tok> = 
    { 
      /// The reduction table
      reductions: (IParseState -> obj) array;
      /// The token number indicating the end of input
      endOfInputTag: int;
      /// A function to compute the tag of a token
      tagOfToken: 'tok -> int;
      /// A function to compute the data carried by a token
      dataOfToken: 'tok -> obj; 
      /// The sparse action table elements
      actionTableElements: uint16[];  
      /// The sparse action table row offsets
      actionTableRowOffsets: uint16[];
      /// The number of symbols for each reduction
      reductionSymbolCounts: uint16[];
      /// The sparse goto table
      gotos: uint16[];
      /// The sparse goto table row offsets
      sparseGotoTableRowOffsets: uint16[];
      /// The sparse table for the productions active for each state
      stateToProdIdxsTableElements: uint16[];  
      /// The sparse table offsets for the productions active for each state
      stateToProdIdxsTableRowOffsets: uint16[];  
      /// This table is logically part of the Goto table
      productionToNonTerminalTable: uint16[];
      /// For fsyacc.exe, this entry is filled in by context from the generated parser file. If no 'parse_error' function
      /// is defined by the user then ParseHelpers.parse_error is used by default (ParseHelpers is opened
      /// at the top of the generated parser file)
      /// This function is used to hold the user specified "parse_error" or "parse_error_rich" functions
      parseError:  ParseErrorContext<'tok> -> unit;
      /// The total number of terminals 
      numTerminals: int;
      /// The tag of the error terminal
      tagOfErrorTerminal: int }

/// Indicates a parse error has occured and parse recovery is in progress
exception RecoverableParseError
/// Indicates an accept action has occured
exception Accept of obj

//-------------------------------------------------------------------------
// Read the tables written by FSYACC.  

type AssocTable(elemTab:uint16[], offsetTab:uint16[]) =
    let cache = new Dictionary<_,_>(2000)

    member t.readAssoc (minElemNum,maxElemNum,defaultValueOfAssoc,keyToFind) =     
        // do a binary chop on the table 
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
        
        // First check the sparse lookaside table
        // Performance note: without this lookaside table the binary chop in readAssoc
        // takes up around 10% of of parsing time 
        // for parsing intensive samples such as the bootstrapped F# compiler.
        //
        // Note: using a .NET Dictionary for this int -> int table looks like it could be sub-optimal.
        // Some other better sparse lookup table may be better.
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

    // Read all entries in the association table
    // Used during error recovery to find all valid entries in the table
    member x.ReadAll(n) =       
        let headOfTable = int offsetTab.[n]
        let firstElemNumber = headOfTable + 1           
        let numberOfElementsInAssoc = int32 elemTab.[headOfTable*2]           
        let defaultValueOfAssoc = int elemTab.[headOfTable*2+1]          
        [ for i in firstElemNumber .. (firstElemNumber+numberOfElementsInAssoc-1) -> 
            (int elemTab.[i*2], int elemTab.[i*2+1]) ], defaultValueOfAssoc

type IdxToIdxListTable(elemTab:uint16[], offsetTab:uint16[]) =

    // Read all entries in a row of the table
    member x.ReadAll(n) =       
        let headOfTable = int offsetTab.[n]
        let firstElemNumber = headOfTable + 1           
        let numberOfElements = int32 elemTab.[headOfTable]           
        [ for i in firstElemNumber .. (firstElemNumber+numberOfElements-1) -> int elemTab.[i] ]

type ValueInfo = 
    struct
        val value: obj
        val startPos: Position
        val endPos: Position
        new (value, startPos, endPos) = { value = value; startPos = startPos; endPos = endPos }
    end

module Implementation =   
    // Definitions shared with fsyacc 
    let anyMarker = 0xffff
    let shiftFlag = 0x0000
    let reduceFlag = 0x4000
    let errorFlag = 0x8000
    let acceptFlag = 0xc000
    let actionMask = 0xc000

    let actionValue action = action &&& (~~~ actionMask)                                    
    let actionKind action = action &&& actionMask

    //-------------------------------------------------------------------------
    // interpret the tables emitted by FSYACC.  

    let interpret (tables: Tables<'tok>) lexer (lexbuf : LexBuffer) initialState =                                                                      
        let localStore = new Dictionary<string,obj>() in
        localStore.["LexBuffer"] <- lexbuf;
        let stateStack : Stack<int> = new Stack<_>(100)
        stateStack.Push(initialState);
        let valueStack = new Stack<ValueInfo>(100)
        let mutable haveLookahead = false                                                                              
        let mutable lookaheadToken = Unchecked.defaultof<'tok>
        let mutable lookaheadEndPos = Unchecked.defaultof<Position>
        let mutable lookaheadStartPos = Unchecked.defaultof<Position>
        let mutable finished = false
        // After an error occurs, we suppress errors until we've shifted three tokens in a row.
        let mutable errorSuppressionCountDown = 0
        
        // When we hit the end-of-file we don't fail straight away but rather keep permitting shift
        // and reduce against the last token in the token stream 20 times or until we've accepted
        // or exhausted the stack. This allows error recovery rules of the form
        //      input : realInput EOF | realInput error EOF | error EOF
        // where consuming one EOF to trigger an error doesn't result in overall parse failure 
        // catastrophe and the loss of intermediate results.
        //
        let mutable inEofCountDown = false
        let mutable eofCountDown = 20 // Number of EOFs to supply at the end for error recovery
        // The 100 here means a maximum of 100 elements for each rule
        let ruleStartPoss = (Array.zeroCreate 100 : Position array)              
        let ruleEndPoss   = (Array.zeroCreate 100 : Position array)              
        let ruleValues    = (Array.zeroCreate 100 : obj array)              
        let lhsPos        = (Array.zeroCreate 2 : Position array)                                            
        let reductions = tables.reductions
        let actionTable = new AssocTable(tables.actionTableElements, tables.actionTableRowOffsets)
        let gotoTable = new AssocTable(tables.gotos, tables.sparseGotoTableRowOffsets)
        let stateToProdIdxsTable = new IdxToIdxListTable(tables.stateToProdIdxsTableElements, tables.stateToProdIdxsTableRowOffsets)

        let parseState =                                                                                            
            { new IParseState with 
                member p.InputRange(n) = ruleStartPoss.[n-1], ruleEndPoss.[n-1]; 
                member p.InputStartPosition(n) = ruleStartPoss.[n-1]
                member p.InputEndPosition(n) = ruleEndPoss.[n-1]; 
                member p.GetInput(n)    = ruleValues.[n-1];        
                member p.ResultRange    = (lhsPos.[0], lhsPos.[1]);  
                member p.ParserLocalStore = (localStore :> IDictionary<_,_>); 
                member p.RaiseError()  = raise RecoverableParseError  (* NOTE: this binding tests the fairly complex logic associated with an object expression implementing a generic abstract method *)
            }       

        // Pop the stack until we can shift the 'error' token. If 'tokenOpt' is given
        // then keep popping until we can shift both the 'error' token and the token in 'tokenOpt'.
        // This is used at end-of-file to make sure we can shift both the 'error' token and the 'EOF' token.
        let rec popStackUntilErrorShifted(tokenOpt) =
            // Keep popping the stack until the "error" terminal is shifted
            if stateStack.Count = 0 then 
                failwith "parse error";
            
            let currState = stateStack.Peek()
            
            let action = actionTable.Read(currState, tables.tagOfErrorTerminal)
            
            if actionKind action = shiftFlag &&  
                (match tokenOpt with 
                 | None -> true
                 | Some(token) -> 
                    let nextState = actionValue action 
                    actionKind (actionTable.Read(nextState, tables.tagOfToken(token))) = shiftFlag) then

                let nextState = actionValue action 
                // The "error" non terminal needs position information, though it tends to be unreliable.
                // Use the StartPos/EndPos from the lex buffer
                valueStack.Push(ValueInfo(box (), lexbuf.StartPos, lexbuf.EndPos));
                stateStack.Push(nextState)
            else
                if valueStack.Count = 0 then 
                    failwith "parse error";
                valueStack.Pop() |> ignore
                stateStack.Pop() |> ignore
                popStackUntilErrorShifted(tokenOpt)

        while not finished do                                                                                    
            if stateStack.Count = 0 then 
                finished <- true
            else
                let state = stateStack.Peek()
                let action = 
                    if not haveLookahead then 
                        if lexbuf.IsPastEndOfStream then 
                            // When the input runs out, keep supplying the last token for eofCountDown times
                            if eofCountDown>0 then
                                haveLookahead <- true
                                eofCountDown <- eofCountDown - 1
                                inEofCountDown <- true
                            else 
                                haveLookahead <- false
                        else 
                            lookaheadToken <- lexer lexbuf
                            lookaheadStartPos <- lexbuf.StartPos
                            lookaheadEndPos <- lexbuf.EndPos
                            haveLookahead <- true;

                    let tag = 
                        if haveLookahead then tables.tagOfToken lookaheadToken 
                        else tables.endOfInputTag   
                                    
                    // Printf.printf "state %d\n" state  
                    actionTable.Read(state,tag)
                        
                let kind = actionKind action 
                if kind = shiftFlag then
                    if errorSuppressionCountDown > 0 then 
                        errorSuppressionCountDown <- errorSuppressionCountDown - 1;
                    let nextState = actionValue action                                     
                    if not haveLookahead then failwith "shift on end of input!";
                    let data = tables.dataOfToken lookaheadToken
                    valueStack.Push(ValueInfo(data, lookaheadStartPos, lookaheadEndPos));
                    stateStack.Push(nextState);                                                                
                    haveLookahead <- false

                elif kind = reduceFlag then
                    let prod = actionValue action                                     
                    let reduction = reductions.[prod]                                                             
                    let n = int tables.reductionSymbolCounts.[prod]
                       // pop the symbols, populate the values and populate the locations                              
                    
                    lhsPos.[0] <- Position.Empty;                                                                     
                    lhsPos.[1] <- Position.Empty;  
                    for i = 0 to n - 1 do                                                                             
                        if valueStack.Count = 0 then failwith "empty symbol stack";
                        let topVal = valueStack.Peek()
                        valueStack.Pop() |> ignore
                        stateStack.Pop() |> ignore
                        ruleValues.[(n-i)-1] <- topVal.value;  
                        ruleStartPoss.[(n-i)-1] <- topVal.startPos;  
                        ruleEndPoss.[(n-i)-1] <- topVal.endPos;  
                        if lhsPos.[1] = Position.Empty then lhsPos.[1] <- topVal.endPos;
                        if not (topVal.startPos = Position.Empty) then lhsPos.[0] <- topVal.startPos
                    
                    try                                                                                               
                          // Printf.printf "reduce %d\n" prod;                                                       
                        let redResult = reduction parseState                                                          
                        valueStack.Push(ValueInfo(redResult, lhsPos.[0], lhsPos.[1]));
                        let currState = stateStack.Peek()
                        let newGotoState = gotoTable.Read(int tables.productionToNonTerminalTable.[prod], currState)
                        stateStack.Push(newGotoState)
                    with                                                                                              
                    | Accept res ->                                                                            
                          finished <- true;                                                                             
                          valueStack.Push(ValueInfo(res, lhsPos.[0], lhsPos.[1])) 
                    | RecoverableParseError ->
                          popStackUntilErrorShifted(None);
                          // User code raised a Parse_error. Don't report errors again until three tokens have been shifted 
                          errorSuppressionCountDown <- 3
                elif kind = errorFlag then
                    // Silently discard inputs and don't report errors 
                    // until three tokens in a row have been shifted 
                    if errorSuppressionCountDown > 0 then 
                        // If we're in the end-of-file count down then we're very keen to 'Accept'.
                        // We can only do this by repeatedly popping the stack until we can shift both an 'error' token
                        // and an EOF token. 
                        if inEofCountDown && eofCountDown < 10 then 
                            popStackUntilErrorShifted(if haveLookahead then Some(lookaheadToken) else None);

                        // If we don't haveLookahead then the end-of-file count down is over and we have no further options.
                        if not haveLookahead then 
                            failwith "parse error: unexpected end of file"
                            
                        // Discard the token
                        haveLookahead <- false
                        // Try again to shift three tokens
                        errorSuppressionCountDown <- 3
                    else

                        let currentToken = if haveLookahead then Some(lookaheadToken) else None
                        let actions,defaultAction = actionTable.ReadAll(state) 
                        let explicit = Set.ofList [ for (tag,_action) in actions -> tag ]
                        
                        let shiftableTokens = 
                           [ for (tag,action) in actions do
                                 if (actionKind action) = shiftFlag then 
                                     yield tag
                             if actionKind defaultAction = shiftFlag  then
                                 for tag in 0 .. tables.numTerminals-1 do  
                                    if not (explicit.Contains(tag)) then 
                                         yield tag ] in
                        let stateStackContent = stateStack.ToArray()
                        let stateStack = Array.sub stateStackContent 0 (min 12 stateStackContent.Length)
                        let reducibleProductions = 
                            [ for state in stateStack do 
                               yield stateToProdIdxsTable.ReadAll(state)  ]

                        let reduceTokens = 
                           [ for (tag,action) in actions do
                                if actionKind(action) = reduceFlag then
                                    yield tag
                             if actionKind(defaultAction) = reduceFlag  then
                                 for tag in 0 .. tables.numTerminals-1 do  
                                    if not (explicit.Contains(tag)) then 
                                         yield tag ] in
                        //let activeRules = stateStack |> List.iter (fun state -> 
                        let errorContext = new ParseErrorContext<'tok>(List.ofArray stateStack, parseState, reduceTokens,currentToken,reducibleProductions, shiftableTokens, "syntax error")
                        tables.parseError(errorContext);
                        popStackUntilErrorShifted(None);
                        errorSuppressionCountDown <- 3;
                    
                elif kind = acceptFlag then 
                    finished <- true

        // OK, we're done - read off the overall generated value
        valueStack.Peek().value

type Tables<'tok> with
    /// Interpret the parser table taking input from the given lexer, using the given lex buffer, and the given start state.
    /// Returns an object indicating the final synthesized value for the parse.
    member tables.Interpret (lexer,lexbuf,initialState) = 
        Implementation.interpret tables lexer lexbuf initialState

/// Helpers used by generated parsers.    
module ParseHelpers =
    /// The default implementation of the parse_error function
    let parse_error (_s:string) = ()
    /// The default implementation of the parse_error_rich function
    let parse_error_rich = (None : (ParseErrorContext<_> -> unit) option)
