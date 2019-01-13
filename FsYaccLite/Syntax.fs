// (c) Microsoft Corporation 2005-2007.
module FsLexYacc.FsYacc.Syntax

type Identifier = string
type Code = string * FsLexYaccLite.Lexing.Position
type Rule = Rule of Identifier list * Identifier option * Code option
type Associativity = LeftAssoc | RightAssoc | NonAssoc

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