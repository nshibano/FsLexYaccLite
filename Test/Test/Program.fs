open Microsoft.FSharp.Text.Lexing
open Test
open Ast
open System

let parse (s : string) =
    let lexbuf = LexBuffer<char>.FromString(s)
    Parser.Expr Lexer.token lexbuf

let show (src : string) =
    let e = parse src
    printfn "%A" e

let case (src : string) (e : Expr) =
    if not (parse src = e) then
        printfn "error (expected success but parse error): \"%s\"" src

let error (src : string) =
    try
        let e = parse src
        printfn "error (expected error but success): \"%s\" => %A" src e
    with _ -> ()

[<EntryPoint>]
let main argv =
    case "x + 1" (Add (Ident "x", Number 1))
    case "1 + 2 * 3" (Add (Number 1, Mult (Number 2, Number 3)))
    case "x * y + z" (Add (Mult (Ident "x",Ident "y"),Ident "z"))
    
    case "x .. y" (Range (Ident "x",Ident "y"))
    error "x .. y .. z"

    case "if 1 then 2" (If (Number 1,Number 2,None))
    case "if 1 then 2 else 3" (If (Number 1,Number 2,Some (Number 3)))
    case "if 1 then if 2 then 3 else 4" (If (Number 1,If (Number 2,Number 3,Some (Number 4)),None))

    case "- 1" (Neg (Number 1))
    case "1 - - 1" (Sub (Number 1,Neg (Number 1)))
    case "- 1 - - - 1" (Sub (Neg (Number 1),Neg (Neg (Number 1))))

    error "+"
    error "1 +"
    error "1 + + 2"

    case
        "if x then if 1 + 2 then a * b + c * d else e .. f + g * h"
        (If
          (Ident "x",
           If
             (Add (Number 1,Number 2),
              Add (Mult (Ident "a",Ident "b"),Mult (Ident "c",Ident "d")),
              Some (Range (Ident "e",Add (Ident "f",Mult (Ident "g",Ident "h"))))),None))
    
    printfn "done"
    Console.ReadKey() |> ignore
    0
