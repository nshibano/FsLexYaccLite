module Lexer
 
open Syntax
open Parser
open System.Text
open FsLexYaccLite.Lexing

let newline (lexbuf:LexBuffer) = lexbuf.EndPos <- lexbuf.EndPos.NextLine


let unexpected_char (lexbuf : LexBuffer) =
  failwith ("Unexpected character '"+(lexbuf.Lexeme)+"'")

let typeDepth = ref 0
let startPos = ref Position_Empty
let mutable str_buf = new System.Text.StringBuilder()

let appendBuf (str:string) = str_buf.Append str |> ignore
let clearBuf () = str_buf <- new System.Text.StringBuilder()


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
let private token_tables = FsLexYaccLite.Lexing.LexTables(token_asciiAlphabetTable, token_nonAsciiCharRangeTable, token_nonAsciiAlphabetTable, token_transitionTable, token_acceptTable)
let private fs_type_asciiAlphabetTable = [| 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 0us; 2us; 1us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us |]
let private fs_type_nonAsciiCharRangeTable = [| 128us |]
let private fs_type_nonAsciiAlphabetTable = [| 2us |]
let private fs_type_transitionTable =
    [|
        [| 1s; 2s; 3s; -1s |]
        [| -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s |]
    |]
let private fs_type_acceptTable = [| -1s; 0s; 1s; 2s |]
let private fs_type_tables = FsLexYaccLite.Lexing.LexTables(fs_type_asciiAlphabetTable, fs_type_nonAsciiCharRangeTable, fs_type_nonAsciiAlphabetTable, fs_type_transitionTable, fs_type_acceptTable)
let private header_asciiAlphabetTable = [| 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 0us; 1us; 12us; 12us; 2us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 0us; 12us; 3us; 12us; 12us; 4us; 12us; 5us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 6us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 12us; 9us; 12us; 12us; 12us; 12us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 12us; 12us; 11us; 12us; 12us |]
let private header_nonAsciiCharRangeTable = [| 128us |]
let private header_nonAsciiAlphabetTable = [| 12us |]
let private header_transitionTable =
    [|
        [| 4s; 2s; 3s; 7s; 1s; 6s; 5s; 4s; 4s; 9s; 4s; 9s; 9s; 8s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 10s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; 11s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 12s; -1s; -1s; -1s; -1s; -1s; -1s; 12s; 12s; -1s; 12s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; 13s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; 14s; -1s; -1s; -1s; -1s; -1s; 15s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 12s; -1s; -1s; -1s; -1s; -1s; -1s; 12s; 12s; -1s; 12s; -1s; -1s; -1s |]
        [| 16s; -1s; -1s; 16s; 16s; 16s; 16s; 16s; 16s; 16s; 16s; 16s; 16s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; 17s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; 18s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 16s; -1s; -1s; 16s; 16s; 16s; 16s; 16s; 16s; 16s; 16s; 16s; 16s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; 17s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
    |]
let private header_acceptTable = [| -1s; 7s; 1s; 7s; 2s; 7s; 7s; 5s; 6s; 7s; 0s; 1s; 2s; 3s; -1s; -1s; 3s; 4s; -1s |]
let private header_tables = FsLexYaccLite.Lexing.LexTables(header_asciiAlphabetTable, header_nonAsciiCharRangeTable, header_nonAsciiAlphabetTable, header_transitionTable, header_acceptTable)
let private code_asciiAlphabetTable = [| 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 0us; 1us; 12us; 12us; 2us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 0us; 12us; 3us; 12us; 12us; 12us; 12us; 4us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 5us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 12us; 8us; 12us; 12us; 12us; 12us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 9us; 10us; 12us; 11us; 12us; 12us |]
let private code_nonAsciiCharRangeTable = [| 128us |]
let private code_nonAsciiAlphabetTable = [| 12us |]
let private code_transitionTable =
    [|
        [| 7s; 3s; 4s; 6s; 5s; 8s; 7s; 7s; 10s; 7s; 2s; 1s; 10s; 9s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; 11s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; 12s; -1s; -1s; -1s; -1s; 13s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 14s; -1s; -1s; -1s; -1s; -1s; 14s; 14s; -1s; 14s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; 15s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 16s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; 17s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 14s; -1s; -1s; -1s; -1s; -1s; 14s; 14s; -1s; 14s; -1s; -1s; -1s; -1s |]
        [| 18s; -1s; -1s; 18s; 18s; 18s; 18s; 18s; 18s; 18s; 18s; 18s; 18s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 16s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 18s; -1s; -1s; 18s; 18s; 18s; 18s; 18s; 18s; 18s; 18s; 18s; 18s; -1s |]
    |]
let private code_acceptTable = [| -1s; 0s; 1s; 2s; 8s; 8s; 4s; 5s; 8s; 7s; 8s; 2s; -1s; -1s; 5s; 6s; 3s; -1s; 6s |]
let private code_tables = FsLexYaccLite.Lexing.LexTables(code_asciiAlphabetTable, code_nonAsciiCharRangeTable, code_nonAsciiAlphabetTable, code_transitionTable, code_acceptTable)
let private codestring_asciiAlphabetTable = [| 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 0us; 1us; 8us; 8us; 2us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 0us; 8us; 3us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 8us; 6us; 8us; 8us; 8us; 8us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 7us; 8us; 8us; 8us; 8us; 8us |]
let private codestring_nonAsciiCharRangeTable = [| 128us |]
let private codestring_nonAsciiAlphabetTable = [| 8us |]
let private codestring_transitionTable =
    [|
        [| 5s; 3s; 4s; 2s; 5s; 5s; 1s; 5s; 7s; 6s |]
        [| -1s; -1s; -1s; 8s; -1s; -1s; 8s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; 9s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 10s; -1s; -1s; -1s; 10s; 10s; -1s; 10s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 10s; -1s; -1s; -1s; 10s; 10s; -1s; 10s; -1s; -1s |]
    |]
let private codestring_acceptTable = [| -1s; 5s; 1s; 2s; 5s; 3s; 4s; 5s; 0s; 2s; 3s |]
let private codestring_tables = FsLexYaccLite.Lexing.LexTables(codestring_asciiAlphabetTable, codestring_nonAsciiCharRangeTable, codestring_nonAsciiAlphabetTable, codestring_transitionTable, codestring_acceptTable)
let private comment_asciiAlphabetTable = [| 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 0us; 5us; 5us; 1us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 2us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 3us; 5us; 5us; 5us; 5us; 4us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us |]
let private comment_nonAsciiCharRangeTable = [| 128us |]
let private comment_nonAsciiAlphabetTable = [| 5us |]
let private comment_transitionTable =
    [|
        [| 2s; 3s; 7s; 4s; 1s; 6s; 5s |]
        [| -1s; -1s; -1s; 8s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 9s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 10s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; 11s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; 11s; -1s |]
    |]
let private comment_acceptTable = [| -1s; 5s; 1s; 5s; 5s; 3s; 4s; 5s; 0s; 1s; 2s; 4s |]
let private comment_tables = FsLexYaccLite.Lexing.LexTables(comment_asciiAlphabetTable, comment_nonAsciiCharRangeTable, comment_nonAsciiAlphabetTable, comment_transitionTable, comment_acceptTable)
let rec token lexbuf =
    match token_tables.Interpret(lexbuf) with
    | 0 ->
        let p = lexbuf.StartPos in header p (new StringBuilder 100) lexbuf 
    | 1 ->
        PERCENT_PERCENT 
    | 2 ->
        typeDepth := 1; startPos := lexbuf.StartPos; clearBuf(); TOKEN (fs_type lexbuf) 
    | 3 ->
        TOKEN (None) 
    | 4 ->
        START 
    | 5 ->
        PREC 
    | 6 ->
        typeDepth := 1; startPos := lexbuf.StartPos; clearBuf(); TYPE (match fs_type lexbuf with Some x -> x | None -> failwith "gettype") 
    | 7 ->
        LEFT 
    | 8 ->
        RIGHT 
    | 9 ->
        NONASSOC 
    | 10 ->
        SEMI 
    | 11 ->
        let p = lexbuf.StartPos in 
        let buff = (new StringBuilder 100) in
        // adjust the first line to get even indentation for all lines w.r.t. the left hand margin
        buff.Append (String.replicate (lexbuf.StartPos.Column+1) " ")  |> ignore;
        code p buff lexbuf 
    | 12 ->
        token lexbuf 
    | 13 ->
        newline lexbuf; token lexbuf 
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
        unexpected_char lexbuf 
    | 20 ->
        EOF  
    | _ -> failwith "token"
and fs_type lexbuf =
    match fs_type_tables.Interpret(lexbuf) with
    | 0 ->
        incr typeDepth; appendBuf(lexbuf.Lexeme); fs_type lexbuf
    | 1 ->
        decr typeDepth; 
        if !typeDepth = 0
        then Some(string str_buf) 
        else appendBuf(lexbuf.Lexeme); fs_type lexbuf 
    | 2 ->
        appendBuf(lexbuf.Lexeme); fs_type lexbuf 
    | _ -> failwith "fs_type"
and header p buff lexbuf =
    match header_tables.Interpret(lexbuf) with
    | 0 ->
        HEADER (buff.ToString()) 
    | 1 ->
        newline lexbuf; 
        ignore <| buff.Append System.Environment.NewLine; 
        header p buff lexbuf 
    | 2 ->
        ignore <| buff.Append (lexbuf.Lexeme); 
        header p buff lexbuf 
    | 3 ->
        ignore <| buff.Append (lexbuf.Lexeme); 
        header p buff lexbuf 
    | 4 ->
        ignore <| buff.Append (lexbuf.Lexeme); 
        header p buff lexbuf 
    | 5 ->
        ignore <| buff.Append (lexbuf.Lexeme); 
        ignore(codestring buff lexbuf); 
        header p buff lexbuf 
    | 6 ->
        EOF 
    | 7 ->
        ignore <| buff.Append(lexbuf.Lexeme).[0]; 
        header p buff lexbuf 
    | _ -> failwith "header"
and code p buff lexbuf =
    match code_tables.Interpret(lexbuf) with
    | 0 ->
        CODE (buff.ToString()) 
    | 1 ->
        ignore <| buff.Append (lexbuf.Lexeme); 
        ignore(code p buff lexbuf); 
        ignore <| buff.Append "}"; 
        code p buff lexbuf 
    | 2 ->
        newline lexbuf; 
        ignore <| buff.Append System.Environment.NewLine; 
        code p buff lexbuf 
    | 3 ->
        ignore <| buff.Append (lexbuf.Lexeme); 
        code p buff lexbuf 
    | 4 ->
        ignore <| buff.Append (lexbuf.Lexeme); 
        ignore(codestring buff lexbuf); 
        code p buff lexbuf 
    | 5 ->
        ignore <| buff.Append (lexbuf.Lexeme); 
        code p buff lexbuf 
    | 6 ->
        ignore <| buff.Append (lexbuf.Lexeme); 
        code p buff lexbuf 
    | 7 ->
        EOF 
    | 8 ->
        ignore <| buff.Append(lexbuf.Lexeme).[0]; 
        code p buff lexbuf 
    | _ -> failwith "code"
and codestring buff lexbuf =
    match codestring_tables.Interpret(lexbuf) with
    | 0 ->
        ignore <| buff.Append (lexbuf.Lexeme); 
        codestring buff lexbuf 
    | 1 ->
        ignore <| buff.Append (lexbuf.Lexeme); 
        buff.ToString() 
    | 2 ->
        newline lexbuf; 
        ignore <| buff.Append System.Environment.NewLine; 
        codestring buff lexbuf 
    | 3 ->
        ignore <| buff.Append (lexbuf.Lexeme); 
        codestring buff lexbuf 
    | 4 ->
        failwith "unterminated string in code" 
    | 5 ->
        ignore <| buff.Append(lexbuf.Lexeme).[0]; 
        codestring buff lexbuf 
    | _ -> failwith "codestring"
and comment lexbuf =
    match comment_tables.Interpret(lexbuf) with
    | 0 ->
        ignore(comment lexbuf); comment lexbuf 
    | 1 ->
        newline lexbuf; comment lexbuf 
    | 2 ->
        () 
    | 3 ->
        failwith "end of file in comment" 
    | 4 ->
        comment lexbuf 
    | 5 ->
        comment lexbuf 
    | _ -> failwith "comment"

