#load "Common.fsx"

open System

open Common

try
    cmd "dotnet" @"build FsLexLite"
    cmd "dotnet" @"build FsYaccLite"
    cp @"Runtime\Lexing.fs" @"Tests\Expr"
    cp @"Runtime\Parsing.fs" @"Tests\Expr"
    cmd "dotnet" @"FsLexLite\bin\Debug\netcoreapp2.1\FsLexLite.dll --unicode --lexlib Microsoft.FSharp.Text.Lexing Tests\Expr\Lexer.fsl"
    cmd "dotnet" @"FsYaccLite\bin\Debug\netcoreapp2.1\FsYaccLite.dll --module Test.Parser --lexlib Microsoft.FSharp.Text.Lexing --parslib Microsoft.FSharp.Text.Parsing -v Tests\Expr\Parser.fsy"
    cmd "dotnet" @"build Tests\Expr"
    cmd "dotnet" @"Tests\Expr\bin\Debug\netcoreapp2.1\Expr.dll"
with
    Failure msg -> Console.Error.WriteLine(msg)
