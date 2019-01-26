#load "Common.fsx"

open System

open Common

try
    cmd "fslexlite" @"FsLexLite\Lexer.fsl"
    cmd "fsyacclite" @"FsLexLite\Parser.fsy"
    cmd "fslexlite" @"FsYaccLite\Lexer.fsl"
    cmd "fsyacclite" @"FsYaccLite\Parser.fsy"
with
    Failure msg -> Console.Error.WriteLine(msg)
