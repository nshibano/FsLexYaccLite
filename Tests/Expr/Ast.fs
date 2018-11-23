module Test.Ast

type Expr =
    | Ident of string
    | Number of int
    | String of string
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mult of Expr * Expr
    | Neg of Expr
    | Cons of Expr * Expr
    | Range of Expr * Expr
    | If of Expr * Expr * Expr option
