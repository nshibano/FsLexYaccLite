module Syntax

type Assoc = LeftAssoc | RightAssoc | NonAssoc

type Decl =
    | Token of string option * string list
    | Type of string * string list
    | Start of string list
    | Prec of Assoc * string list

type Clause =
    { Symbols : string list
      PrecSymbol : string option
      Code : string }

type Rule =
    { Ident : string
      Type : string option
      Clauses : Clause list }

type ParserSpec = 
    { Header : string
      Decls : Decl list
      Rules : Rule list }
      
let stringOfAssoc (assoc : Assoc) =
    match assoc with
    | LeftAssoc -> "left"
    | RightAssoc -> "right"
    | NonAssoc -> "nonassoc"