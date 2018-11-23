// Implementation file for parser generated by fsyacc
module FsLexYaccLite.Lex.Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "fslexpars.fsy"

(* (c) Microsoft Corporation 2005-2008.  *)
open FsLexYaccLite.Lex
open FsLexYaccLite.Lex.Syntax

# 12 "fslexpars.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | BAR
  | DOT
  | PLUS
  | STAR
  | QMARK
  | EQUALS
  | UNDERSCORE
  | LBRACK
  | RBRACK
  | HAT
  | DASH
  | RULE
  | PARSE
  | LET
  | AND
  | LPAREN
  | RPAREN
  | CHAR of (char)
  | CODE of (FsLexYaccLite.Lex.Syntax.Code)
  | STRING of (string)
  | IDENT of (string)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_BAR
    | TOKEN_DOT
    | TOKEN_PLUS
    | TOKEN_STAR
    | TOKEN_QMARK
    | TOKEN_EQUALS
    | TOKEN_UNDERSCORE
    | TOKEN_LBRACK
    | TOKEN_RBRACK
    | TOKEN_HAT
    | TOKEN_DASH
    | TOKEN_RULE
    | TOKEN_PARSE
    | TOKEN_LET
    | TOKEN_AND
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_CHAR
    | TOKEN_CODE
    | TOKEN_STRING
    | TOKEN_IDENT
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startspec
    | NONTERM_spec
    | NONTERM_codeopt
    | NONTERM_Macros
    | NONTERM_macro
    | NONTERM_Rules
    | NONTERM_rule
    | NONTERM_args
    | NONTERM_optbar
    | NONTERM_clauses
    | NONTERM_clause
    | NONTERM_regexp
    | NONTERM_CharSet

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | BAR  -> 1 
  | DOT  -> 2 
  | PLUS  -> 3 
  | STAR  -> 4 
  | QMARK  -> 5 
  | EQUALS  -> 6 
  | UNDERSCORE  -> 7 
  | LBRACK  -> 8 
  | RBRACK  -> 9 
  | HAT  -> 10 
  | DASH  -> 11 
  | RULE  -> 12 
  | PARSE  -> 13 
  | LET  -> 14 
  | AND  -> 15 
  | LPAREN  -> 16 
  | RPAREN  -> 17 
  | CHAR _ -> 18 
  | CODE _ -> 19 
  | STRING _ -> 20 
  | IDENT _ -> 21 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_BAR 
  | 2 -> TOKEN_DOT 
  | 3 -> TOKEN_PLUS 
  | 4 -> TOKEN_STAR 
  | 5 -> TOKEN_QMARK 
  | 6 -> TOKEN_EQUALS 
  | 7 -> TOKEN_UNDERSCORE 
  | 8 -> TOKEN_LBRACK 
  | 9 -> TOKEN_RBRACK 
  | 10 -> TOKEN_HAT 
  | 11 -> TOKEN_DASH 
  | 12 -> TOKEN_RULE 
  | 13 -> TOKEN_PARSE 
  | 14 -> TOKEN_LET 
  | 15 -> TOKEN_AND 
  | 16 -> TOKEN_LPAREN 
  | 17 -> TOKEN_RPAREN 
  | 18 -> TOKEN_CHAR 
  | 19 -> TOKEN_CODE 
  | 20 -> TOKEN_STRING 
  | 21 -> TOKEN_IDENT 
  | 24 -> TOKEN_end_of_input
  | 22 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startspec 
    | 1 -> NONTERM_spec 
    | 2 -> NONTERM_codeopt 
    | 3 -> NONTERM_codeopt 
    | 4 -> NONTERM_Macros 
    | 5 -> NONTERM_Macros 
    | 6 -> NONTERM_macro 
    | 7 -> NONTERM_Rules 
    | 8 -> NONTERM_Rules 
    | 9 -> NONTERM_rule 
    | 10 -> NONTERM_args 
    | 11 -> NONTERM_args 
    | 12 -> NONTERM_optbar 
    | 13 -> NONTERM_optbar 
    | 14 -> NONTERM_clauses 
    | 15 -> NONTERM_clauses 
    | 16 -> NONTERM_clause 
    | 17 -> NONTERM_regexp 
    | 18 -> NONTERM_regexp 
    | 19 -> NONTERM_regexp 
    | 20 -> NONTERM_regexp 
    | 21 -> NONTERM_regexp 
    | 22 -> NONTERM_regexp 
    | 23 -> NONTERM_regexp 
    | 24 -> NONTERM_regexp 
    | 25 -> NONTERM_regexp 
    | 26 -> NONTERM_regexp 
    | 27 -> NONTERM_regexp 
    | 28 -> NONTERM_regexp 
    | 29 -> NONTERM_regexp 
    | 30 -> NONTERM_CharSet 
    | 31 -> NONTERM_CharSet 
    | 32 -> NONTERM_CharSet 
    | 33 -> NONTERM_CharSet 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 24 
let _fsyacc_tagOfErrorTerminal = 22

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | BAR  -> "BAR" 
  | DOT  -> "DOT" 
  | PLUS  -> "PLUS" 
  | STAR  -> "STAR" 
  | QMARK  -> "QMARK" 
  | EQUALS  -> "EQUALS" 
  | UNDERSCORE  -> "UNDERSCORE" 
  | LBRACK  -> "LBRACK" 
  | RBRACK  -> "RBRACK" 
  | HAT  -> "HAT" 
  | DASH  -> "DASH" 
  | RULE  -> "RULE" 
  | PARSE  -> "PARSE" 
  | LET  -> "LET" 
  | AND  -> "AND" 
  | LPAREN  -> "LPAREN" 
  | RPAREN  -> "RPAREN" 
  | CHAR _ -> "CHAR" 
  | CODE _ -> "CODE" 
  | STRING _ -> "STRING" 
  | IDENT _ -> "IDENT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | BAR  -> (null : System.Object) 
  | DOT  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | STAR  -> (null : System.Object) 
  | QMARK  -> (null : System.Object) 
  | EQUALS  -> (null : System.Object) 
  | UNDERSCORE  -> (null : System.Object) 
  | LBRACK  -> (null : System.Object) 
  | RBRACK  -> (null : System.Object) 
  | HAT  -> (null : System.Object) 
  | DASH  -> (null : System.Object) 
  | RULE  -> (null : System.Object) 
  | PARSE  -> (null : System.Object) 
  | LET  -> (null : System.Object) 
  | AND  -> (null : System.Object) 
  | LPAREN  -> (null : System.Object) 
  | RPAREN  -> (null : System.Object) 
  | CHAR _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | CODE _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | STRING _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | IDENT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 2us; 65535us; 0us; 2us; 5us; 6us; 2us; 65535us; 2us; 3us; 8us; 9us; 2us; 65535us; 2us; 8us; 8us; 8us; 2us; 65535us; 4us; 5us; 15us; 16us; 2us; 65535us; 4us; 14us; 15us; 14us; 2us; 65535us; 17us; 18us; 23us; 24us; 1us; 65535us; 20us; 21us; 2us; 65535us; 21us; 22us; 27us; 28us; 2us; 65535us; 21us; 26us; 27us; 26us; 10us; 65535us; 12us; 13us; 13us; 36us; 21us; 29us; 27us; 29us; 29us; 36us; 36us; 36us; 37us; 36us; 38us; 36us; 42us; 37us; 43us; 38us; 2us; 65535us; 45us; 46us; 48us; 49us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 6us; 9us; 12us; 15us; 18us; 21us; 23us; 26us; 29us; 40us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 1us; 2us; 1us; 5us; 1us; 5us; 1us; 6us; 1us; 6us; 1us; 6us; 6us; 6us; 22us; 23us; 24us; 25us; 26us; 2us; 7us; 8us; 1us; 7us; 1us; 7us; 1us; 9us; 1us; 9us; 1us; 9us; 1us; 9us; 1us; 9us; 1us; 9us; 1us; 11us; 1us; 11us; 1us; 13us; 2us; 14us; 15us; 1us; 14us; 1us; 14us; 6us; 16us; 22us; 23us; 24us; 25us; 26us; 1us; 16us; 1us; 17us; 1us; 18us; 1us; 19us; 1us; 20us; 1us; 21us; 6us; 22us; 22us; 23us; 24us; 25us; 26us; 6us; 22us; 23us; 24us; 25us; 26us; 26us; 6us; 22us; 23us; 24us; 25us; 26us; 27us; 1us; 23us; 1us; 24us; 1us; 25us; 1us; 26us; 1us; 27us; 1us; 27us; 2us; 28us; 29us; 3us; 28us; 30us; 31us; 1us; 28us; 1us; 29us; 3us; 29us; 30us; 31us; 1us; 29us; 2us; 30us; 31us; 1us; 31us; 1us; 31us; 2us; 32us; 33us; 1us; 33us; 1us; 33us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 10us; 12us; 14us; 16us; 18us; 20us; 22us; 24us; 26us; 33us; 36us; 38us; 40us; 42us; 44us; 46us; 48us; 50us; 52us; 54us; 56us; 58us; 61us; 63us; 65us; 72us; 74us; 76us; 78us; 80us; 82us; 84us; 91us; 98us; 105us; 107us; 109us; 111us; 113us; 115us; 117us; 120us; 124us; 126us; 128us; 132us; 134us; 137us; 139us; 141us; 144us; 146us; |]
let _fsyacc_action_rows = 57
let _fsyacc_actionTableElements = [|1us; 16387us; 19us; 7us; 0us; 49152us; 1us; 16388us; 14us; 10us; 1us; 32768us; 12us; 4us; 1us; 32768us; 21us; 17us; 1us; 16387us; 19us; 7us; 0us; 16385us; 0us; 16386us; 1us; 16388us; 14us; 10us; 0us; 16389us; 1us; 32768us; 21us; 11us; 1us; 32768us; 6us; 12us; 7us; 32768us; 0us; 32us; 7us; 33us; 8us; 45us; 16us; 43us; 18us; 31us; 20us; 34us; 21us; 35us; 11us; 16390us; 0us; 32us; 1us; 42us; 3us; 39us; 4us; 40us; 5us; 41us; 7us; 33us; 8us; 45us; 16us; 43us; 18us; 31us; 20us; 34us; 21us; 35us; 1us; 16392us; 15us; 15us; 1us; 32768us; 21us; 17us; 0us; 16391us; 1us; 16394us; 21us; 23us; 1us; 32768us; 6us; 19us; 1us; 32768us; 13us; 20us; 1us; 16396us; 1us; 25us; 7us; 32768us; 0us; 32us; 7us; 33us; 8us; 45us; 16us; 43us; 18us; 31us; 20us; 34us; 21us; 35us; 0us; 16393us; 1us; 16394us; 21us; 23us; 0us; 16395us; 0us; 16397us; 1us; 16399us; 1us; 27us; 7us; 32768us; 0us; 32us; 7us; 33us; 8us; 45us; 16us; 43us; 18us; 31us; 20us; 34us; 21us; 35us; 0us; 16398us; 12us; 32768us; 0us; 32us; 1us; 42us; 3us; 39us; 4us; 40us; 5us; 41us; 7us; 33us; 8us; 45us; 16us; 43us; 18us; 31us; 19us; 30us; 20us; 34us; 21us; 35us; 0us; 16400us; 0us; 16401us; 0us; 16402us; 0us; 16403us; 0us; 16404us; 0us; 16405us; 10us; 16406us; 0us; 32us; 3us; 39us; 4us; 40us; 5us; 41us; 7us; 33us; 8us; 45us; 16us; 43us; 18us; 31us; 20us; 34us; 21us; 35us; 10us; 16410us; 0us; 32us; 3us; 39us; 4us; 40us; 5us; 41us; 7us; 33us; 8us; 45us; 16us; 43us; 18us; 31us; 20us; 34us; 21us; 35us; 12us; 32768us; 0us; 32us; 1us; 42us; 3us; 39us; 4us; 40us; 5us; 41us; 7us; 33us; 8us; 45us; 16us; 43us; 17us; 44us; 18us; 31us; 20us; 34us; 21us; 35us; 0us; 16407us; 0us; 16408us; 0us; 16409us; 7us; 32768us; 0us; 32us; 7us; 33us; 8us; 45us; 16us; 43us; 18us; 31us; 20us; 34us; 21us; 35us; 7us; 32768us; 0us; 32us; 7us; 33us; 8us; 45us; 16us; 43us; 18us; 31us; 20us; 34us; 21us; 35us; 0us; 16411us; 2us; 32768us; 10us; 48us; 18us; 54us; 2us; 32768us; 9us; 47us; 18us; 51us; 0us; 16412us; 1us; 32768us; 18us; 54us; 2us; 32768us; 9us; 50us; 18us; 51us; 0us; 16413us; 1us; 16414us; 11us; 52us; 1us; 32768us; 18us; 53us; 0us; 16415us; 1us; 16416us; 11us; 55us; 1us; 32768us; 18us; 56us; 0us; 16417us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 2us; 3us; 5us; 7us; 9us; 11us; 12us; 13us; 15us; 16us; 18us; 20us; 28us; 40us; 42us; 44us; 45us; 47us; 49us; 51us; 53us; 61us; 62us; 64us; 65us; 66us; 68us; 76us; 77us; 90us; 91us; 92us; 93us; 94us; 95us; 96us; 107us; 118us; 131us; 132us; 133us; 134us; 142us; 150us; 151us; 154us; 157us; 158us; 160us; 163us; 164us; 166us; 168us; 169us; 171us; 173us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 5us; 1us; 0us; 0us; 2us; 4us; 3us; 1us; 6us; 0us; 2us; 0us; 1us; 3us; 1us; 2us; 1us; 1us; 1us; 1us; 1us; 2us; 2us; 2us; 2us; 3us; 3us; 3us; 4us; 2us; 4us; 1us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 3us; 3us; 4us; 5us; 5us; 6us; 7us; 7us; 8us; 8us; 9us; 9us; 10us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 12us; 12us; 12us; 12us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 65535us; 65535us; 65535us; 16385us; 16386us; 65535us; 16389us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16391us; 65535us; 65535us; 65535us; 65535us; 65535us; 16393us; 65535us; 16395us; 16397us; 65535us; 65535us; 16398us; 65535us; 16400us; 16401us; 16402us; 16403us; 16404us; 16405us; 65535us; 65535us; 65535us; 16407us; 16408us; 16409us; 65535us; 65535us; 16411us; 65535us; 65535us; 16412us; 65535us; 65535us; 16413us; 65535us; 65535us; 16415us; 65535us; 65535us; 16417us; |]
let _fsyacc_reductions ()  =    [| 
# 238 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : FsLexYaccLite.Lex.Syntax.Spec)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startspec));
# 247 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'codeopt)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'Macros)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'Rules)) in
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : 'codeopt)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 22 "fslexpars.fsy"
                                                               { TopCode=_1;Macros=_2;Rules=_4;BottomCode=_5 } 
                   )
# 22 "fslexpars.fsy"
                 : FsLexYaccLite.Lex.Syntax.Spec));
# 261 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : FsLexYaccLite.Lex.Syntax.Code)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 23 "fslexpars.fsy"
                                     _1 
                   )
# 23 "fslexpars.fsy"
                 : 'codeopt));
# 272 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 23 "fslexpars.fsy"
                                              "", (parseState.ResultRange |> fst) 
                   )
# 23 "fslexpars.fsy"
                 : 'codeopt));
# 282 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 24 "fslexpars.fsy"
                                [] 
                   )
# 24 "fslexpars.fsy"
                 : 'Macros));
# 292 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'macro)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'Macros)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 24 "fslexpars.fsy"
                                                      _1 :: _2 
                   )
# 24 "fslexpars.fsy"
                 : 'Macros));
# 304 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'regexp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 25 "fslexpars.fsy"
                                                      (_2, _4) 
                   )
# 25 "fslexpars.fsy"
                 : 'macro));
# 316 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'rule)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Rules)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 26 "fslexpars.fsy"
                                             _1 :: _3 
                   )
# 26 "fslexpars.fsy"
                 : 'Rules));
# 328 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'rule)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 26 "fslexpars.fsy"
                                                                 [_1] 
                   )
# 26 "fslexpars.fsy"
                 : 'Rules));
# 339 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'args)) in
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : 'optbar)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : 'clauses)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 27 "fslexpars.fsy"
                                                                    (_1,_2,_6) 
                   )
# 27 "fslexpars.fsy"
                 : 'rule));
# 353 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 28 "fslexpars.fsy"
                             [] 
                   )
# 28 "fslexpars.fsy"
                 : 'args));
# 363 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'args)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 28 "fslexpars.fsy"
                                                 _1 :: _2 
                   )
# 28 "fslexpars.fsy"
                 : 'args));
# 375 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 29 "fslexpars.fsy"
                               
                   )
# 29 "fslexpars.fsy"
                 : 'optbar));
# 385 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 29 "fslexpars.fsy"
                                         
                   )
# 29 "fslexpars.fsy"
                 : 'optbar));
# 395 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'clause)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'clauses)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "fslexpars.fsy"
                                                  _1 :: _3 
                   )
# 30 "fslexpars.fsy"
                 : 'clauses));
# 407 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'clause)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "fslexpars.fsy"
                                                                        [_1] 
                   )
# 30 "fslexpars.fsy"
                 : 'clauses));
# 418 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'regexp)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : FsLexYaccLite.Lex.Syntax.Code)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 "fslexpars.fsy"
                                           _1, _2 
                   )
# 31 "fslexpars.fsy"
                 : 'clause));
# 430 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : char)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "fslexpars.fsy"
                                                            Inp (CharSet [SingleChar _1]) 
                   )
# 33 "fslexpars.fsy"
                 : 'regexp));
# 441 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "fslexpars.fsy"
                                                            Inp Eof 
                   )
# 34 "fslexpars.fsy"
                 : 'regexp));
# 451 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 "fslexpars.fsy"
                                                            Inp Any 
                   )
# 35 "fslexpars.fsy"
                 : 'regexp));
# 461 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 "fslexpars.fsy"
                                                            Seq (List.init _1.Length (fun i -> Inp (CharSet [SingleChar _1.[i]]))) 
                   )
# 36 "fslexpars.fsy"
                 : 'regexp));
# 472 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "fslexpars.fsy"
                                                            Macro _1 
                   )
# 37 "fslexpars.fsy"
                 : 'regexp));
# 483 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'regexp)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'regexp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "fslexpars.fsy"
                                                            Seq [_1; _2] 
                   )
# 38 "fslexpars.fsy"
                 : 'regexp));
# 495 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'regexp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "fslexpars.fsy"
                                                            Seq [_1; Star _1] 
                   )
# 39 "fslexpars.fsy"
                 : 'regexp));
# 506 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'regexp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 40 "fslexpars.fsy"
                                                            Star _1 
                   )
# 40 "fslexpars.fsy"
                 : 'regexp));
# 517 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'regexp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 41 "fslexpars.fsy"
                                                            Alt [Seq []; _1] 
                   )
# 41 "fslexpars.fsy"
                 : 'regexp));
# 528 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'regexp)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'regexp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "fslexpars.fsy"
                                                            Alt [_1; _3] 
                   )
# 42 "fslexpars.fsy"
                 : 'regexp));
# 540 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'regexp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "fslexpars.fsy"
                                                            _2 
                   )
# 43 "fslexpars.fsy"
                 : 'regexp));
# 551 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'CharSet)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "fslexpars.fsy"
                                                            Inp (CharSet (List.rev _2)) 
                   )
# 44 "fslexpars.fsy"
                 : 'regexp));
# 562 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'CharSet)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "fslexpars.fsy"
                                                            Inp (NotCharSet (List.rev _3)) 
                   )
# 45 "fslexpars.fsy"
                 : 'regexp));
# 573 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'CharSet)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : char)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "fslexpars.fsy"
                                                    SingleChar _2 :: _1 
                   )
# 48 "fslexpars.fsy"
                 : 'CharSet));
# 585 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'CharSet)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : char)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : char)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "fslexpars.fsy"
                                                    CharRange (_2, _4) :: _1 
                   )
# 49 "fslexpars.fsy"
                 : 'CharSet));
# 598 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : char)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "fslexpars.fsy"
                                                    SingleChar _1 :: [] 
                   )
# 50 "fslexpars.fsy"
                 : 'CharSet));
# 609 "fslexpars.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : char)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : char)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "fslexpars.fsy"
                                                    CharRange (_1, _3) :: [] 
                   )
# 51 "fslexpars.fsy"
                 : 'CharSet));
|]
# 622 "fslexpars.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 25;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let spec lexer lexbuf : FsLexYaccLite.Lex.Syntax.Spec =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
