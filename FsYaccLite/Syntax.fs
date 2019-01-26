module Syntax

type Identifier = string
type Code = string

type Associativity = LeftAssoc | RightAssoc | NonAssoc

type Decl =
    | Token of string option * Identifier list
    | Type of string * Identifier list
    | Start of Identifier list
    | Prec of Associativity * Identifier list

type Rule =
    { Symbols : string list
      PrecSymbol : string option
      Code : Code }

type ParserSpec = 
    { Header : Code
      Decls : Decl list
      //Tokens : (Identifier * string option) list
      //Types : (Identifier * string) list
      //Associativities : (Identifier * Associativity) list list
      //StartSymbols : Identifier list
      Rules : (Identifier * string option * Rule list) list }
      
let stringOfAssoc (assoc : Associativity) =
    match assoc with
    | LeftAssoc -> "left"
    | RightAssoc -> "right"
    | NonAssoc -> "nonassoc"