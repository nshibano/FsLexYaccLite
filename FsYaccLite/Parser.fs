// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64"

open Syntax

type token = 
  | IDENT of string
  | CODE of Syntax.Code
  | HEADER of Syntax.Code
  | ERROR
  | EOF
  | SEMI
  | PREC
  | COLON
  | GREATER
  | LESS
  | NONASSOC
  | RIGHT
  | LEFT
  | START
  | PERCENT_PERCENT
  | BAR
  | TYPE of string
  | TOKEN of string option

let tagOfToken (t : token) = 
  match t with
  | IDENT _ -> 0 
  | CODE _ -> 1 
  | HEADER _ -> 2 
  | ERROR  -> 3 
  | EOF  -> 4 
  | SEMI  -> 5 
  | PREC  -> 6 
  | COLON  -> 7 
  | GREATER  -> 8 
  | LESS  -> 9 
  | NONASSOC  -> 10 
  | RIGHT  -> 11 
  | LEFT  -> 12 
  | START  -> 13 
  | PERCENT_PERCENT  -> 14 
  | BAR  -> 15 
  | TYPE _ -> 16 
  | TOKEN _ -> 17 

let endOfInputTag = 20 

let dataOfToken (t : token) : obj = 
  match t with 
  | IDENT x -> box x 
  | CODE x -> box x 
  | HEADER x -> box x 
  | ERROR  -> null 
  | EOF  -> null 
  | SEMI  -> null 
  | PREC  -> null 
  | COLON  -> null 
  | GREATER  -> null 
  | LESS  -> null 
  | NONASSOC  -> null 
  | RIGHT  -> null 
  | LEFT  -> null 
  | START  -> null 
  | PERCENT_PERCENT  -> null 
  | BAR  -> null 
  | TYPE x -> box x 
  | TOKEN x -> box x 

let reductionSymbolCounts = [|1us; 5us; 1us; 0us; 2us; 0us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 0us; 2us; 1us; 5us; 0us; 1us; 0us; 1us; 3us; 1us; 3us; 2us; 2us; 0us; 0us; 2us|]
let productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 3us; 3us; 4us; 4us; 4us; 4us; 4us; 4us; 5us; 5us; 6us; 6us; 7us; 8us; 8us; 9us; 9us; 10us; 10us; 11us; 12us; 12us; 12us; 13us; 13us|]
let maxProductionBodyLength = 5
let actionTable_buckets = [| -1s; -1s; 0s; -1s; -1s; 1s; 2s; 3s; 4s; 5s; 6s; 7s; 11s; 14s; 15s; -1s; 16s; 18s; 19s; -1s; -1s; -1s; -1s; 20s; -1s; -1s; 21s; 22s; 24s; 25s; 26s; -1s; 27s; 29s; 30s; -1s; -1s; 31s; -1s; -1s; -1s; -1s; -1s; 32s; 33s; -1s; 34s; 35s; 36s; 38s; 40s; 41s; -1s; 42s; 44s; -1s; -1s; -1s; 45s; -1s; -1s; -1s; 46s; -1s; 47s; 48s; -1s; 49s; -1s; -1s; 50s; -1s; -1s; 51s; 52s; 54s; 55s; 56s; -1s; 57s; 59s; -1s; -1s; -1s; -1s; 60s; 61s; -1s; 62s; 63s; 64s; 66s; 69s; 70s; 72s; 73s; 75s; 78s; 79s; -1s; 80s; 82s; 83s; -1s; -1s; 84s; 86s; -1s; -1s; -1s; 87s; 89s; 91s; 92s; 93s; 94s; 95s; 98s; 100s; 101s; 102s; 103s; 104s; 106s; -1s; 107s; 108s; -1s; 109s; -1s; -1s |]
let actionTable_entries = [| 2s; 1s; 136s; -14s; 137s; -14s; 138s; -14s; 139s; -14s; 140s; -14s; 10s; -4s; -12s; -4s; -143s; -14s; -274s; 22s; 666s; 37s; -13s; -4s; -144s; -14s; 798s; 41s; 13s; -4s; 14s; -4s; -17s; -4s; 147s; -14s; 17s; -4s; 673s; -28s; 678s; 38s; 157s; -14s; -159s; -14s; 420s; 22s; 159s; -14s; 160s; -14s; 161s; -14s; -164s; -14s; 294s; 22s; 164s; -14s; 820s; 42s; 168s; -14s; 567s; 28s; 568s; -27s; 570s; 29s; 178s; -14s; -180s; -14s; 441s; 22s; -181s; -14s; 573s; -27s; 181s; -14s; 182s; -14s; -185s; -14s; 315s; 22s; 185s; -14s; 189s; 17s; 62s; 32767s; 588s; 28s; 589s; -27s; 591s; 29s; 594s; -27s; 73s; -6s; -75s; -6s; 336s; 22s; 75s; -6s; 76s; -6s; 77s; -6s; -80s; -6s; 210s; -14s; 80s; -6s; 609s; 28s; 610s; -27s; 612s; 29s; 220s; -14s; -222s; -14s; 483s; -18s; -223s; -14s; -485s; -18s; 615s; -27s; 223s; -14s; -225s; -14s; 486s; -18s; 94s; 5s; -96s; 6s; 226s; -14s; -97s; 7s; -228s; -14s; 489s; -18s; 97s; 8s; 98s; 9s; -101s; 10s; 231s; -14s; 101s; 11s; 364s; 23s; -106s; -14s; 498s; 26s; 630s; -20s; -242s; -14s; 634s; -20s; -243s; -14s; 635s; 35s; 243s; -14s; 244s; -14s; 245s; -14s; 115s; -14s; -117s; -14s; -248s; -14s; 378s; 17s; -118s; -14s; 248s; -14s; 118s; -14s; 119s; -14s; 382s; 24s; 121s; -14s; -123s; -14s; 777s; 28s; 778s; -27s; 780s; 29s; 126s; -14s; 783s; -27s |]
let actionTable_defaultActions = [| -32768s; -3s; -1s; -32768s; -32768s; -32768s; -32768s; -32768s; -32768s; -32768s; -32768s; -32768s; -5s; -12s; -11s; -10s; -9s; -32768s; -32768s; -16s; -8s; -7s; -13s; -32768s; -2s; -15s; -19s; -32768s; -32768s; -32768s; -32768s; -23s; -32768s; -25s; -26s; -21s; -17s; -32768s; -32768s; -32768s; -22s; -29s; -24s |]
let gotoTable_buckets = [| 0s; 1s; 3s; -1s; -1s; -1s; 6s; 7s; 8s; -1s; -1s; 9s; 11s; 13s; 14s; -1s; 15s; 18s; -1s; -1s; -1s; -1s; -1s; 20s; -1s; -1s; 21s; 22s; -1s |]
let gotoTable_entries = [| 145s; 20s; -2s; 2s; 117s; 16s; -3s; 3s; -61s; 12s; 89s; 14s; 528s; 40s; 529s; 31s; 530s; 32s; -331s; 27s; 388s; 30s; -390s; 31s; 418s; 34s; 390s; 32s; 159s; 21s; -46s; 4s; -104s; 15s; 132s; 18s; -76s; 13s; 133s; 19s; 429s; 36s; 461s; 39s; -260s; 25s; 404s; 33s |]
let reductions =    [| 
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.ParserSpec)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      failwith "unreachable"
                   )
                 : '_startspec));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'headeropt)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'decls)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'rules)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                                                       { Header = _1; Decls = List.rev _2; Rules = List.rev _4 } 
                   )
                 : Syntax.ParserSpec));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.Code)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                    _1 
                   )
                 : 'headeropt));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                    "", (fst parseState.ResultRange)
                   )
                 : 'headeropt));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'decls)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'decl)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                        _2 :: _1 
                   )
                 : 'decls));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                        [] 
                   )
                 : 'decls));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string option)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'idents)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                             Token (_1, List.rev _2) 
                   )
                 : 'decl));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'idents)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                             Type (_1, List.rev _2) 
                   )
                 : 'decl));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'idents)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                             Start (List.rev _2) 
                   )
                 : 'decl));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'idents)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                             Prec (LeftAssoc, List.rev _2) 
                   )
                 : 'decl));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'idents)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                             Prec (RightAssoc, List.rev _2) 
                   )
                 : 'decl));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'idents)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                             Prec (NonAssoc, List.rev _2) 
                   )
                 : 'decl));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'idents)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                          _2 :: _1 
                   )
                 : 'idents));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                          [] 
                   )
                 : 'idents));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'rules)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'rule)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                        _2 :: _1 
                   )
                 : 'rules));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'rule)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                        [_1] 
                   )
                 : 'rules));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'optbar)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'clauses)) in
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : 'optsemi)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                                                (_1,_4) 
                   )
                 : 'rule));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                               
                   )
                 : 'optbar));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                         
                   )
                 : 'optbar));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                
                   )
                 : 'optsemi));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                           
                   )
                 : 'optsemi));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'clause)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'clauses)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                                  _1 :: _3 
                   )
                 : 'clauses));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'clause)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                                                        [_1] 
                   )
                 : 'clauses));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'syms)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'optprec)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Syntax.Code)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                                 Rule(_1,_2,Some _3) 
                   )
                 : 'clause));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'syms)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                        _1 :: _2 
                   )
                 : 'syms));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'syms)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                                                  "error" :: _2 
                   )
                 : 'syms));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                                                                      [] 
                   )
                 : 'syms));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                None 
                   )
                 : 'optprec));
        (fun (parseState : FsLexYaccLite.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                                                      Some _2 
                   )
                 : 'optprec));
|]
let terminalsCount = 21
let nonTerminalsCount = 14
let tables = FsLexYaccLite.Parsing.ParseTables(reductions, endOfInputTag, tagOfToken, dataOfToken, reductionSymbolCounts, productionToNonTerminalTable, maxProductionBodyLength, gotoTable_buckets, gotoTable_entries, nonTerminalsCount, actionTable_buckets, actionTable_entries, actionTable_defaultActions, terminalsCount)
let spec lexer lexbuf : Syntax.ParserSpec = unbox (tables.Interpret(lexer, lexbuf, 0))
