module FsLexYaccLite.Lex.Syntax

open Microsoft.FSharp.Text.Lexing

type Ident = string
type Code = string * Position

type Alphabet = int

type CharSetItem =
    | SingleChar of char
    | CharRange of first : char * last : char

type Input =
    // after parsing
    | CharSet of CharSetItem list
    | NotCharSet of CharSetItem list
    | Any 
    | Eof
    // after char to alphabet translation
    | Alphabet of Alphabet

type Regexp = 
    | Inp of Input
    | Alt of Regexp list
    | Seq of Regexp list
    | Star of Regexp
    | Macro of Ident

type Clause = Regexp * Code

type Spec = 
    { TopCode: Code
      Macros: (Ident * Regexp) list
      Rules: (Ident * Ident list * Clause list) list
      BottomCode: Code }
