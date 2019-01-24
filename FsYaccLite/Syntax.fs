module Syntax

type Identifier = string
type Code = string * FsLexYaccLite.Lexing.Position
type Rule = Rule of Identifier list * Identifier option * Code option

type Associativity = LeftAssoc | RightAssoc | NonAssoc

type Decl =
    | Token of string list
    | Type of string list
    | Start of string list
    | Left of string list
    | Right of string list
    | Nonassoc of string list

type ParserSpec = 
    { Header : Code
      Tokens : (Identifier * string option) list
      Types : (Identifier * string) list
      Associativities : (Identifier * Associativity) list list
      StartSymbols : Identifier list
      Rules : (Identifier * Rule list) list }
      
let stringOfAssoc (assoc : Associativity) =
    match assoc with
    | LeftAssoc -> "left"
    | RightAssoc -> "right"
    | NonAssoc -> "nonassoc"