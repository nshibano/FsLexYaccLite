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

let case (src : string) (e1 : Expr) =
    try
        let e2 = parse src
        if e1 <> e2 then
            printfn "error (parse success but wrong result): \"%s\"" src
    with _ -> printfn "error (expected success but parse error): \"%s\"" src

let error (src : string) =
    try
        let e = parse src
        printfn "error (expected parse error but success): \"%s\" => %A" src e
    with _ -> ()

[<EntryPoint>]
let main argv =
    case "x + y" (Add (Ident "x", Ident "y"))
    case "1 + 2 + 3" (Add (Add (Number 1,Number 2),Number 3))

    case "1 * 2" (Mult (Number 1,Number 2))
    case "a * b * c" (Mult (Mult (Ident "a",Ident "b"),Ident "c"))
    
    case "1 + 2 * 3 + 4 * 5" (Add (Add (Number 1,Mult (Number 2,Number 3)),Mult (Number 4,Number 5)))
    case "a * b + c * d + e" (Add (Add (Mult (Ident "a",Ident "b"),Mult (Ident "c",Ident "d")),Ident "e"))
    
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
