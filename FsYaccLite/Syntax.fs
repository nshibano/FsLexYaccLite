// (c) Microsoft Corporation 2005-2007.
module FsLexYacc.FsYacc.Syntax

open System
open System.Collections.Generic
open Printf
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Text.Lexing
open System.Diagnostics

type Identifier = string
type Code = string * Position
type Rule = Rule of Identifier list * Identifier option * Code option
type Associativity = LeftAssoc | RightAssoc | NonAssoc

type ParserSpec= 
    { Header : Code
      Tokens : (Identifier * string option) list
      Types : (Identifier * string) list
      Associativities : (Identifier * Associativity) list list
      StartSymbols : Identifier list
      Rules : (Identifier * Rule list) list }
      
