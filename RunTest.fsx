﻿#load "Common.fsx"

open System

open Common

try
    cmd "dotnet" @"build FsLexLite"
    cmd "dotnet" @"build FsYaccLite"
    cp @"Runtime\FsLexYaccLiteRuntime.fs" @"Tests\Expr"
    cmd "dotnet" @"FsLexLite\bin\Debug\netcoreapp2.1\FsLexLite.dll Tests\Expr\Lexer.fsl"
    cmd "dotnet" @"FsYaccLite\bin\Debug\netcoreapp2.1\FsYaccLite.dll -v Tests\Expr\Parser.fsy"
    cmd "dotnet" @"build Tests\Expr"
    cmd "dotnet" @"Tests\Expr\bin\Debug\netcoreapp2.1\Expr.dll"
with
    Failure msg -> Console.Error.WriteLine(msg)
