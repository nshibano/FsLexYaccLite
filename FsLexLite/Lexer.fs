module Lexer 
open Compile
open Parser
open FsLexYaccLite.Lexing

open System.Text

let escape c = 
  match c with
  | '\\' -> '\\'
  | '\'' -> '\''
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'b' -> '\b'
  | 'r' -> '\r'
  | c -> c

let lexeme (lexbuf : LexBuffer) = lexbuf.Lexeme  
let newline (lexbuf:LexBuffer) = lexbuf.EndPos <- lexbuf.EndPos.NextLine

let unexpected_char lexbuf =
      failwith ("Unexpected character '"+(lexeme lexbuf)+"'")

let digit d = 
      if d >= '0' && d <= '9' then int32 d - int32 '0'   
      else failwith "digit" 

let hexdigit d = 
      if d >= '0' && d <= '9' then digit d 
      else if d >= 'a' && d <= 'f' then int32 d - int32 'a' + 10
      else if d >= 'A' && d <= 'F' then int32 d - int32 'A' + 10
      else failwithf "bad hexdigit: %c" d 

let trigraph c1 c2 c3 =
      char (digit c1 * 100 + digit c2 * 10 + digit c3)

let hexgraph c1 c2 =
      char (hexdigit c1 * 16 + hexdigit c2)

let unicodegraph_short (s:string) =
    if s.Length <> 4 then failwith "unicodegraph";
    char(hexdigit s.[0] * 4096 + hexdigit s.[1] * 256 + hexdigit s.[2] * 16 + hexdigit s.[3])

let unicodegraph_long (s:string) =
    if s.Length <> 8 then failwith "unicodegraph_long";
    let high = hexdigit s.[0] * 4096 + hexdigit s.[1] * 256 + hexdigit s.[2] * 16 + hexdigit s.[3] in 
    let low = hexdigit s.[4] * 4096 + hexdigit s.[5] * 256 + hexdigit s.[6] * 16 + hexdigit s.[7] in 
    if high = 0 then None, char low 
    else 
      (* A surrogate pair - see http://www.unicode.org/unicode/uni2book/ch03.pdf, section 3.7 *)
      Some (char(0xD800 + ((high * 0x10000 + low - 0x10000) / 0x400))),
      char(0xDF30 + ((high * 0x10000 + low - 0x10000) % 0x400))


let private token_asciiAlphabetTable = [| 41us; 41us; 41us; 41us; 41us; 41us; 41us; 41us; 41us; 0us; 1us; 41us; 41us; 2us; 41us; 41us; 41us; 41us; 41us; 41us; 41us; 41us; 41us; 41us; 41us; 41us; 41us; 41us; 41us; 41us; 41us; 41us; 0us; 41us; 3us; 41us; 41us; 41us; 41us; 4us; 5us; 6us; 7us; 8us; 41us; 9us; 10us; 11us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 41us; 41us; 41us; 13us; 41us; 14us; 41us; 15us; 15us; 15us; 15us; 15us; 15us; 16us; 16us; 16us; 16us; 16us; 16us; 16us; 16us; 16us; 16us; 16us; 16us; 16us; 16us; 17us; 16us; 16us; 16us; 16us; 16us; 18us; 19us; 20us; 21us; 22us; 41us; 23us; 24us; 25us; 26us; 27us; 28us; 29us; 29us; 29us; 29us; 29us; 30us; 29us; 31us; 32us; 33us; 29us; 34us; 35us; 36us; 37us; 29us; 29us; 38us; 29us; 29us; 39us; 40us; 41us; 41us; 41us |]
let private token_nonAsciiCharRangeTable = [| 128us |]
let private token_nonAsciiAlphabetTable = [| 41us |]
let private token_transitionTable =
    [|
        [| 9s; 10s; 11s; 8s; 6s; 21s; 22s; 16s; 15s; 25s; 14s; 26s; 27s; 18s; 17s; 12s; 12s; 12s; 19s; 27s; 20s; 24s; 23s; 5s; 12s; 12s; 12s; 3s; 12s; 12s; 4s; 12s; 12s; 2s; 1s; 12s; 12s; 12s; 12s; 7s; 13s; 27s; 28s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 29s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 31s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 32s; 30s; 30s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 33s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 34s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 36s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; 35s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 37s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; 38s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; 39s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 40s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 41s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 42s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 30s; 43s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 44s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 45s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 46s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; 35s; 35s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 47s; -1s; -1s; -1s; -1s; 50s; -1s; 35s; -1s; -1s; -1s; -1s; 35s; -1s; -1s; -1s; -1s; -1s; -1s; 35s; -1s; -1s; 35s; -1s; 35s; 49s; 48s; -1s; -1s; -1s; -1s |]
        [| 37s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 51s; -1s; -1s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 52s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 53s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 54s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 55s; -1s; -1s; 55s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 55s; 55s; 55s; 55s; 55s; 55s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 56s; -1s; -1s; 56s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 56s; 56s; 56s; 56s; 56s; 56s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 57s; -1s; -1s; 57s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 57s; 57s; 57s; 57s; 57s; 57s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 51s; -1s; -1s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; 51s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 58s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 59s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 60s; -1s; -1s; 60s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 60s; 60s; 60s; 60s; 60s; 60s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 61s; -1s; -1s; 61s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 61s; 61s; 61s; 61s; 61s; 61s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 62s; -1s; -1s; 62s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 62s; 62s; 62s; 62s; 62s; 62s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 30s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 30s; -1s; -1s; 30s; 30s; 30s; -1s; -1s; -1s; -1s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; 30s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 63s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 64s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 65s; -1s; -1s; 65s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 65s; 65s; 65s; 65s; 65s; 65s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 66s; -1s; -1s; 66s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 66s; 66s; 66s; 66s; 66s; 66s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 67s; -1s; -1s; 67s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 67s; 67s; 67s; 67s; 67s; 67s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 68s; -1s; -1s; 68s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 68s; 68s; 68s; 68s; 68s; 68s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 69s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 70s; -1s; -1s; 70s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 70s; 70s; 70s; 70s; 70s; 70s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 71s; -1s; -1s; 71s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 71s; 71s; 71s; 71s; 71s; 71s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 72s; -1s; -1s; 72s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 72s; 72s; 72s; 72s; 72s; 72s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 73s; -1s; -1s; 73s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; 73s; 73s; 73s; 73s; 73s; 73s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 74s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
    |]
let private token_acceptTable = [| -1s; 14s; 14s; 14s; 14s; 14s; 30s; 10s; 11s; 12s; 13s; 30s; 14s; 15s; 16s; 17s; 18s; 19s; 20s; 21s; 22s; 23s; 24s; 25s; 26s; 27s; 30s; 30s; 31s; 14s; 14s; 14s; 14s; 14s; 14s; -1s; -1s; 12s; 13s; 28s; 29s; 14s; 14s; 2s; 3s; 4s; 5s; -1s; -1s; -1s; -1s; 29s; 0s; 14s; -1s; -1s; -1s; -1s; 1s; -1s; -1s; -1s; -1s; 6s; 7s; -1s; -1s; -1s; -1s; 8s; -1s; -1s; -1s; -1s; 9s |]
let private token_tables = FsLexYaccLite.Lexing.UnicodeTables(token_asciiAlphabetTable, token_nonAsciiCharRangeTable, token_nonAsciiAlphabetTable, token_transitionTable, token_acceptTable)
let private string_asciiAlphabetTable = [| 13us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 0us; 1us; 13us; 13us; 2us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 0us; 13us; 3us; 13us; 13us; 13us; 13us; 4us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 13us; 13us; 13us; 13us; 13us; 13us; 13us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 13us; 7us; 13us; 13us; 13us; 13us; 8us; 9us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 10us; 8us; 8us; 8us; 11us; 8us; 12us; 8us; 8us; 8us; 8us; 8us; 8us; 13us; 13us; 13us; 13us; 13us |]
let private string_nonAsciiCharRangeTable = [| 128us |]
let private string_nonAsciiAlphabetTable = [| 13us |]
let private string_transitionTable =
    [|
        [| 5s; 3s; 4s; 2s; 7s; 5s; 5s; 1s; 5s; 5s; 5s; 5s; 5s; 7s; 6s |]
        [| -1s; 8s; 9s; 10s; 10s; 11s; -1s; 10s; -1s; 10s; 10s; 10s; 10s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; 12s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 13s; -1s; -1s; -1s; -1s; 13s; 13s; -1s; 13s; 13s; 13s; 13s; 13s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; 8s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; 14s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 13s; -1s; -1s; -1s; -1s; 13s; 13s; -1s; 13s; 13s; 13s; 13s; 13s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; 15s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
    |]
let private string_acceptTable = [| -1s; 7s; 3s; 4s; 7s; 5s; 6s; 7s; 0s; -1s; 1s; -1s; 4s; 5s; -1s; 2s |]
let private string_tables = FsLexYaccLite.Lexing.UnicodeTables(string_asciiAlphabetTable, string_nonAsciiCharRangeTable, string_nonAsciiAlphabetTable, string_transitionTable, string_acceptTable)
let private code_asciiAlphabetTable = [| 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 0us; 1us; 11us; 11us; 2us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 0us; 11us; 3us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 4us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 5us; 11us; 11us; 11us; 11us; 11us; 11us; 11us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 6us; 11us; 7us; 11us; 11us; 11us; 11us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 8us; 9us; 11us; 10us; 11us; 11us |]
let private code_nonAsciiCharRangeTable = [| 128us |]
let private code_nonAsciiAlphabetTable = [| 11us |]
let private code_transitionTable =
    [|
        [| 7s; 5s; 6s; 4s; 8s; 7s; 7s; 3s; 7s; 2s; 1s; 10s; 9s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; 11s; -1s; -1s; -1s; 11s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; 12s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 13s; -1s; -1s; -1s; -1s; 13s; 13s; -1s; 13s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; 14s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 13s; -1s; -1s; -1s; -1s; 13s; 13s; -1s; 13s; -1s; -1s; -1s; -1s |]
        [| 15s; -1s; -1s; 15s; 15s; 15s; 15s; 15s; 15s; 15s; 15s; 15s; -1s |]
        [| 15s; -1s; -1s; 15s; 15s; 15s; 15s; 15s; 15s; 15s; 15s; 15s; -1s |]
    |]
let private code_acceptTable = [| -1s; 0s; 1s; 8s; 3s; 4s; 8s; 5s; 8s; 7s; 8s; 2s; 4s; 5s; 6s; 6s |]
let private code_tables = FsLexYaccLite.Lexing.UnicodeTables(code_asciiAlphabetTable, code_nonAsciiCharRangeTable, code_nonAsciiAlphabetTable, code_transitionTable, code_acceptTable)
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
let private codestring_tables = FsLexYaccLite.Lexing.UnicodeTables(codestring_asciiAlphabetTable, codestring_nonAsciiCharRangeTable, codestring_nonAsciiAlphabetTable, codestring_transitionTable, codestring_acceptTable)
let private comment_asciiAlphabetTable = [| 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 0us; 12us; 12us; 1us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 2us; 12us; 12us; 12us; 12us; 3us; 4us; 5us; 6us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 7us; 12us; 12us; 12us; 12us; 12us; 8us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 9us; 12us; 12us; 12us; 10us; 12us; 11us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us |]
let private comment_nonAsciiCharRangeTable = [| 128us |]
let private comment_nonAsciiAlphabetTable = [| 12us |]
let private comment_transitionTable =
    [|
        [| 4s; 5s; 2s; 1s; 3s; 9s; 6s; 8s; 8s; 8s; 8s; 8s; 8s; 7s |]
        [| 10s; 10s; 10s; 10s; 10s; 10s; 10s; 11s; 10s; 10s; 10s; 10s; 10s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; 12s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| 13s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; 14s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; 15s; 15s; 15s; 15s; 15s; 15s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; 16s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; 10s; 10s; -1s; -1s; -1s; 10s; 10s; 10s; 10s; 10s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; 15s; 15s; 15s; 15s; 15s; 15s; -1s |]
        [| -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s; -1s |]
    |]
let private comment_acceptTable = [| -1s; 7s; 1s; 7s; 3s; 7s; 7s; 5s; 6s; 7s; -1s; -1s; 2s; 3s; 4s; 6s; 0s |]
let private comment_tables = FsLexYaccLite.Lexing.UnicodeTables(comment_asciiAlphabetTable, comment_nonAsciiCharRangeTable, comment_nonAsciiAlphabetTable, comment_transitionTable, comment_acceptTable)
let rec token lexbuf =
    match token_tables.Interpret(lexbuf) with
    | 0 ->
        RULE 
    | 1 ->
        PARSE 
    | 2 ->
        EOF 
    | 3 ->
        LET 
    | 4 ->
        AND 
    | 5 ->
        let s = lexeme lexbuf in 
        CHAR (if s.[1] = '\\' then escape s.[2] else s.[1])  
    | 6 ->
        let s = lexeme lexbuf in 
        CHAR (trigraph s.[2] s.[3] s.[4]) 
    | 7 ->
        let s = lexeme lexbuf in 
        CHAR (hexgraph s.[3] s.[4]) 
    | 8 ->
        let s = lexeme lexbuf in 
        CHAR (unicodegraph_short s.[3..6]) 
    | 9 ->
        let s = lexeme lexbuf in 
        match (unicodegraph_long s.[3..10]) with 
        | None, c -> CHAR(c)
        | Some _ , _ -> failwith "Unicode characters needing surrogate pairs are not yet supported by this tool" 
    | 10 ->
        let p = lexbuf.StartPos in 
        let buff = (new StringBuilder 100) in
        // adjust the first line to get even indentation for all lines w.r.t. the left hand margin
        buff.Append (String.replicate (lexbuf.StartPos.Column+1) " ") |> ignore;
        code p buff lexbuf 
    | 11 ->
        string  lexbuf.StartPos (new StringBuilder 100) lexbuf 
    | 12 ->
        token lexbuf 
    | 13 ->
        newline lexbuf; token lexbuf 
    | 14 ->
        IDENT (lexeme lexbuf) 
    | 15 ->
        BAR 
    | 16 ->
        DOT 
    | 17 ->
        PLUS 
    | 18 ->
        STAR 
    | 19 ->
        QMARK 
    | 20 ->
        EQUALS 
    | 21 ->
        LBRACK 
    | 22 ->
        RBRACK 
    | 23 ->
        LPAREN 
    | 24 ->
        RPAREN 
    | 25 ->
        UNDERSCORE 
    | 26 ->
        HAT 
    | 27 ->
        DASH 
    | 28 ->
        ignore(comment lexbuf.StartPos lexbuf); token lexbuf 
    | 29 ->
        token lexbuf 
    | 30 ->
        unexpected_char lexbuf 
    | 31 ->
        ENDE 
    | _ -> failwith "token"
and string p buff lexbuf =
    match string_tables.Interpret(lexbuf) with
    | 0 ->
        newline lexbuf; string p buff lexbuf 
    | 1 ->
        let _ = buff.Append (escape (lexeme lexbuf).[1]) in
        string p buff lexbuf 
    | 2 ->
        let s = lexeme lexbuf in 
        let _ = buff.Append (trigraph s.[1] s.[2] s.[3]) in
        string p buff lexbuf  
    | 3 ->
        STRING (buff.ToString()) 
    | 4 ->
        newline lexbuf; 
        let _ = buff.Append System.Environment.NewLine in
        string p buff lexbuf 
    | 5 ->
        let _ = buff.Append (lexeme lexbuf) in 
        string p buff lexbuf 
    | 6 ->
        failwith (Printf.sprintf "end of file in string started at (%d,%d)" p.Line (p.AbsoluteOffset - p.StartOfLine))  
    | 7 ->
        let _ = buff.Append (lexeme lexbuf).[0] in
        string p buff lexbuf 
    | _ -> failwith "string"
and code p buff lexbuf =
    match code_tables.Interpret(lexbuf) with
    | 0 ->
        CODE (buff.ToString(), p) 
    | 1 ->
        let _ = buff.Append (lexeme lexbuf) in 
        ignore(code p buff lexbuf); 
        let _ = buff.Append "}" in
        code p buff lexbuf 
    | 2 ->
        let _ = buff.Append (lexeme lexbuf) in 
        code p buff lexbuf 
    | 3 ->
        let _ = buff.Append (lexeme lexbuf) in 
        ignore(codestring buff lexbuf); 
        code p buff lexbuf 
    | 4 ->
        newline lexbuf; 
        let _ = buff.Append System.Environment.NewLine in
        code p buff lexbuf 
    | 5 ->
        let _ = buff.Append (lexeme lexbuf) in 
        code p buff lexbuf 
    | 6 ->
        let _ = buff.Append (lexeme lexbuf) in
        code p buff lexbuf 
    | 7 ->
        failwith (Printf.sprintf "end of file in code started at (%d,%d)" p.Line (p.AbsoluteOffset - p.StartOfLine))  
    | 8 ->
        let _ = buff.Append (lexeme lexbuf).[0] in
        code p buff lexbuf 
    | _ -> failwith "code"
and codestring buff lexbuf =
    match codestring_tables.Interpret(lexbuf) with
    | 0 ->
        let _ = buff.Append (lexeme lexbuf) in 
        codestring buff lexbuf 
    | 1 ->
        let _ = buff.Append (lexeme lexbuf) in 
        buff.ToString() 
    | 2 ->
        newline lexbuf; 
        let _ = buff.Append System.Environment.NewLine in
        codestring buff lexbuf 
    | 3 ->
        let _ = buff.Append (lexeme lexbuf) in 
        codestring buff lexbuf 
    | 4 ->
        failwith "unterminated string in code" 
    | 5 ->
        let _ = buff.Append (lexeme lexbuf).[0] in
        codestring buff lexbuf 
    | _ -> failwith "codestring"
and comment p lexbuf =
    match comment_tables.Interpret(lexbuf) with
    | 0 ->
        comment p lexbuf 
    | 1 ->
        ignore(try string lexbuf.StartPos (new StringBuilder 100) lexbuf 
               with Failure s -> failwith (s + "\n" + Printf.sprintf "error while processing string nested in comment started at (%d,%d)" p.Line (p.AbsoluteOffset - p.StartOfLine))); 
        comment p lexbuf 
    | 2 ->
        ignore(try comment p lexbuf with Failure s -> failwith (s + "\n" + Printf.sprintf "error while processing nested comment started at (%d,%d)" p.Line (p.AbsoluteOffset - p.StartOfLine))); 
        comment p lexbuf 
    | 3 ->
        newline lexbuf; comment p lexbuf 
    | 4 ->
        () 
    | 5 ->
        failwith (Printf.sprintf "end of file in comment started at (%d,%d)" p.Line (p.AbsoluteOffset - p.StartOfLine))  
    | 6 ->
        comment p lexbuf 
    | 7 ->
        comment p lexbuf 
    | _ -> failwith "comment"

