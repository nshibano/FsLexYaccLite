module FsLexYaccLite.Lex.Syntax

open System.Collections.Generic
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Text.Lexing

let (|KeyValue|) (kvp:KeyValuePair<_,_>) = kvp.Key,kvp.Value

type Ident = string
type Code = string * Position

type Alphabet = uint32

type Input =
  | Alphabet of Alphabet
  | UnicodeCategory of string 
  | Any 
  | NotCharSet of Set<Alphabet>

type Regexp = 
  | Alt of Regexp list
  | Seq of Regexp list
  | Inp of Input
  | Star of Regexp
  | Macro of Ident

type Clause = Regexp * Code

type Spec = 
    { TopCode: Code;
      Macros: (Ident * Regexp) list;
      Rules: (Ident * Ident list * Clause list) list;
      BottomCode: Code }


let Eof : Alphabet = 0xFFFFFFFEu
let Epsilon : Alphabet = 0xFFFFFFFFu