open FsLexYaccLite.Lexing
open System
open System.Collections.Generic
open Test
open Ast

let getTokens (s : string) = 
    let lexbuf = LexBuffer.FromString(s)
    let accu = List()
    while not lexbuf.IsPastEndOfStream do
        try
            accu.Add(Lexer.token lexbuf)
        with _ -> lexbuf.IsPastEndOfStream <- true
    accu.ToArray()

let parse (s : string) =
    let lexbuf = LexBuffer.FromString(s)
    Parser.Start Lexer.token lexbuf

// Use this function to print parse result and then
// turn it into test case by copying the result displayed in the console to this script.
let show (src : string) =
    let e = parse src
    printfn "%A" e

let mutable ok = true

let case (src : string) (e1 : Expr) =
    try
        let e2 = parse src
        if e1 <> e2 then
            ok <- false
            printfn "error (parse success but wrong result): \"%s\"" src
    with _ ->
        ok <- false
        printfn "error (expected success but parse error): \"%s\"" src
        printfn "%A" (getTokens src)

let error (src : string) =
    try
        let e = parse src
        ok <- false
        printfn "error (expected parse error but success): \"%s\" => %A" src e
    with _ -> ()

[<EntryPoint>]
let main argv =

    //let lexbuf = LexBuffer.FromString("x + y")
    //for i = 1 to 3 do
    //    printfn "%A" (Lexer.token lexbuf)

    case "1" (Number 1)
    case "x" (Ident "x")

    case "x + y" (Add (Ident "x", Ident "y"))
    case "1 + 2 + 3" (Add (Add (Number 1,Number 2),Number 3))

    case "1 * 2" (Mult (Number 1,Number 2))
    case "a * b * c" (Mult (Mult (Ident "a",Ident "b"),Ident "c"))
    
    case "1 + 2 * 3 + 4 * 5" (Add (Add (Number 1,Mult (Number 2,Number 3)),Mult (Number 4,Number 5)))
    case "a * b + c * d + e" (Add (Add (Mult (Ident "a",Ident "b"),Mult (Ident "c",Ident "d")),Ident "e"))

    case "l :: m" (Cons (Ident "l",Ident "m"))
    case "l :: m :: n" (Cons (Ident "l",Cons (Ident "m",Ident "n")))
    
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
        "if x then if 1 + 2 then a * b + - c * d :: l else e .. f + g * h"
        (If
          (Ident "x",
           If
             (Add (Number 1,Number 2),
              Cons
                (Add (Mult (Ident "a",Ident "b"),Mult (Neg (Ident "c"),Ident "d")),
                 Ident "l"),
              Some (Range (Ident "e",Add (Ident "f",Mult (Ident "g",Ident "h"))))),None))

    case  """ "hello" + "string" """ (Add (String "hello",String "string"))
    case  """ "こんにちは" + "世界" """ (Add (String "こんにちは",String "世界"))

    printfn "done (%s)" (if ok then "success" else "failure")

    //Console.ReadKey() |> ignore

    if ok then 0 else -1
