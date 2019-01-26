module Lexer
 
open Syntax
open Parser
open System.Text
open FsLexYaccLiteRuntime

let append (lexbuf : LexBuffer) (sb : StringBuilder) = sb.Append(lexbuf.Lexeme) |> ignore

let private token_asciiAlphabetTable = [| 31us; 31us; 31us; 31us; 31us; 31us; 31us; 31us; 31us; 0us; 1us; 31us; 31us; 2us; 31us; 31us; 31us; 31us; 31us; 31us; 31us; 31us; 31us; 31us; 31us; 31us; 31us; 31us; 31us; 31us; 31us; 31us; 0us; 31us; 31us; 31us; 31us; 3us; 31us; 4us; 31us; 31us; 5us; 31us; 31us; 31us; 31us; 6us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 8us; 9us; 10us; 31us; 31us; 31us; 31us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 31us; 31us; 31us; 31us; 4us; 31us; 12us; 13us; 14us; 13us; 15us; 16us; 17us; 18us; 19us; 13us; 20us; 21us; 13us; 22us; 23us; 24us; 13us; 25us; 26us; 27us; 13us; 13us; 13us; 13us; 28us; 13us; 29us; 30us; 31us; 31us; 31us |]
let private token_nonAsciiCharRangeTable = [| 128us |]
let private token_nonAsciiAlphabetTable = [| 31us |]
let private token_transitionTable =
    [|
        [| 4s; 5s; 6s; 1s; 11s; 11s; 9s; 11s; 10s; 2s; 11s; 7s; 7s; 7s; 7s; 7s; 7s; 7s; 7s; 7s; 7s; 7s; 7s; 7s; 7s; 7s; 7s; 7s; 7s; 3s; 8s; 11s; 12s |]
        [| -1s; -1s; -1s; 14s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 18s; 20s; -1s; 17s; 19s; 16s; 15s; -1s; 13s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 21s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; 22s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 23s; -1s; -1s; 23s; -1s; -1s; -1s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; 24s; 25s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 26s; -1s; -1s; -1s; -1s; 27s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 28s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 29s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 31s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 32s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 21s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 23s; -1s; -1s; 23s; -1s; -1s; -1s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; 23s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 33s; -1s; -1s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 34s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 35s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 36s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 37s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 38s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 39s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 40s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 33s; -1s; -1s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; 33s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 41s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 42s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 43s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 44s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 45s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 46s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 47s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 48s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 50s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 49s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 51s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 52s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 53s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 55s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 54s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 50s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 49s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 56s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 55s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 54s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 57s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 58s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
    |]
let private token_acceptTable = [| -1s; 19s; 10s; 11s; 12s; 13s; 19s; 14s; 15s; 19s; 18s; 19s; 20s; 0s; 1s; -1s; -1s; -1s; -1s; -1s; -1s; 12s; 13s; 14s; 16s; 17s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 17s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 5s; 7s; -1s; -1s; 3s; 6s; -1s; 4s; 8s; -1s; 2s; -1s; -1s; -1s; 9s |]
let private token_tables = FsLexYaccLiteRuntime.LexTables(token_asciiAlphabetTable, token_nonAsciiCharRangeTable, token_nonAsciiAlphabetTable, token_transitionTable, token_acceptTable)
let private fs_type_asciiAlphabetTable = [| 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 0us; 2us; 1us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us |]
let private fs_type_nonAsciiCharRangeTable = [| 128us |]
let private fs_type_nonAsciiAlphabetTable = [| 2us |]
let private fs_type_transitionTable =
    [|
        [| 2s; 1s; 3s; 4s |]
        [| -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s |]
    |]
let private fs_type_acceptTable = [| -1s; 0s; 1s; 2s; 3s |]
let private fs_type_tables = FsLexYaccLiteRuntime.LexTables(fs_type_asciiAlphabetTable, fs_type_nonAsciiCharRangeTable, fs_type_nonAsciiAlphabetTable, fs_type_transitionTable, fs_type_acceptTable)
let private header_asciiAlphabetTable = [| 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 0us; 4us; 4us; 1us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 2us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 3us; 4us; 4us |]
let private header_nonAsciiCharRangeTable = [| 128us |]
let private header_nonAsciiAlphabetTable = [| 4us |]
let private header_transitionTable =
    [|
        [| 2s; 3s; 1s; 4s; 4s; 5s |]
        [| -1s; -1s; -1s; 6s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 7s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
    |]
let private header_acceptTable = [| -1s; 2s; 1s; 2s; 2s; 3s; 0s; 1s |]
let private header_tables = FsLexYaccLiteRuntime.LexTables(header_asciiAlphabetTable, header_nonAsciiCharRangeTable, header_nonAsciiAlphabetTable, header_transitionTable, header_acceptTable)
let private code_asciiAlphabetTable = [| 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 0us; 4us; 4us; 1us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 2us; 4us; 3us; 4us; 4us |]
let private code_nonAsciiCharRangeTable = [| 128us |]
let private code_nonAsciiAlphabetTable = [| 4us |]
let private code_transitionTable =
    [|
        [| 3s; 4s; 2s; 1s; 5s; 6s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 7s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
    |]
let private code_acceptTable = [| -1s; 0s; 1s; 2s; 3s; 3s; 4s; 2s |]
let private code_tables = FsLexYaccLiteRuntime.LexTables(code_asciiAlphabetTable, code_nonAsciiCharRangeTable, code_nonAsciiAlphabetTable, code_transitionTable, code_acceptTable)
let private comment_asciiAlphabetTable = [| 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 0us; 4us; 4us; 1us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 2us; 4us; 4us; 4us; 4us; 3us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us |]
let private comment_nonAsciiCharRangeTable = [| 128us |]
let private comment_nonAsciiAlphabetTable = [| 4us |]
let private comment_transitionTable =
    [|
        [| 3s; 4s; 2s; 1s; 5s; 6s |]
        [| -1s; -1s; 7s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; 8s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 9s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 10s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 10s; -1s |]
    |]
let private comment_acceptTable = [| -1s; 3s; 3s; 2s; 3s; 3s; 4s; 0s; 1s; 2s; 3s |]
let private comment_tables = FsLexYaccLiteRuntime.LexTables(comment_asciiAlphabetTable, comment_nonAsciiCharRangeTable, comment_nonAsciiAlphabetTable, comment_transitionTable, comment_acceptTable)
let rec token lexbuf =
    match token_tables.Interpret(lexbuf) with
    | 0 ->
        header (StringBuilder()) lexbuf 
    | 1 ->
        PERCENT_PERCENT 
    | 2 ->
        TOKEN (fs_type 1 (StringBuilder()) lexbuf) 
    | 3 ->
        TOKEN (None) 
    | 4 ->
        START 
    | 5 ->
        PREC 
    | 6 ->
        TYPE (match fs_type 1 (StringBuilder()) lexbuf with Some x -> x | None -> failwith "gettype") 
    | 7 ->
        LEFT 
    | 8 ->
        RIGHT 
    | 9 ->
        NONASSOC 
    | 10 ->
        SEMI 
    | 11 ->
        let buf = StringBuilder()
        buf.Append(' ', lexbuf.StartPos.Column + 1) |> ignore
        code 1 buf lexbuf 
    | 12 ->
        token lexbuf 
    | 13 ->
        lexbuf.NewLine(); token lexbuf 
    | 14 ->
        IDENT lexbuf.Lexeme 
    | 15 ->
        BAR 
    | 16 ->
        ignore(comment lexbuf); token lexbuf 
    | 17 ->
        token lexbuf 
    | 18 ->
        COLON 
    | 19 ->
        failwith "Unexpected char %c" lexbuf.Lexeme.[0] 
    | 20 ->
        EOF 
    | _ -> failwith "token"
and fs_type level buf lexbuf =
    match fs_type_tables.Interpret(lexbuf) with
    | 0 ->
        if level = 1 then
            Some (buf.ToString()) 
        else
            append lexbuf buf
            fs_type (level - 1) buf lexbuf 
    | 1 ->
        append lexbuf buf
        fs_type (level + 1) buf lexbuf 
    | 2 ->
        append lexbuf buf
        fs_type level buf lexbuf 
    | 3 ->
        failwith "Unterminated type expression literal" 
    | _ -> failwith "fs_type"
and header buf lexbuf =
    match header_tables.Interpret(lexbuf) with
    | 0 ->
        HEADER (buf.ToString()) 
    | 1 ->
        lexbuf.NewLine()
        append lexbuf buf
        header buf lexbuf 
    | 2 ->
        append lexbuf buf
        header buf lexbuf 
    | 3 ->
        failwith "Unterminated header" 
    | _ -> failwith "header"
and code level buf lexbuf =
    match code_tables.Interpret(lexbuf) with
    | 0 ->
        if level = 1 then
            CODE (buf.ToString())
        else
            append lexbuf buf
            code (level - 1) buf lexbuf 
    | 1 ->
        append lexbuf buf
        code (level + 1) buf lexbuf 
    | 2 ->
        lexbuf.NewLine()
        append lexbuf buf
        code level buf lexbuf 
    | 3 ->
        append lexbuf buf
        code level buf lexbuf 
    | 4 ->
        failwith "Unterminated code" 
    | _ -> failwith "code"
and comment lexbuf =
    match comment_tables.Interpret(lexbuf) with
    | 0 ->
        comment lexbuf; comment lexbuf 
    | 1 ->
        () 
    | 2 ->
        lexbuf.NewLine(); comment lexbuf 
    | 3 ->
        comment lexbuf 
    | 4 ->
        failwith "Unterminated comment" 
    | _ -> failwith "comment"

