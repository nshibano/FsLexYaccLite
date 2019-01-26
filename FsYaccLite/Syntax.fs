module Syntax

type Identifier = string
type Code = string
type Rule = Rule of Identifier list * Identifier option * Code option

type Associativity = LeftAssoc | RightAssoc | NonAssoc

type Decl =
    | Token of string option * Identifier list
    | Type of string * Identifier list
    | Start of Identifier list
    | Prec of Associativity * Identifier list

type ParserSpec = 
    { Header : Code
      Decls : Decl list
      //Tokens : (Identifier * string option) list
      //Types : (Identifier * string) list
      //Associativities : (Identifier * Associativity) list list
      //StartSymbols : Identifier list
      Rules : (Identifier * Rule list) list }
      
let stringOfAssoc (assoc : Associativity) =
    match assoc with
    | LeftAssoc -> "left"
    | RightAssoc -> "right"
    | NonAssoc -> "nonassoc"