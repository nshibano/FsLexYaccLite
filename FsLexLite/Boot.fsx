#load "../Common.fsx"
open Common

cp @"..\Runtime\Lexing.fs" "."
cp @"..\Runtime\Lexing.fs" "."

cmd "fslexlite" "Lexer.fsl"
cmd "fsyacclite" "Parser.fsy"