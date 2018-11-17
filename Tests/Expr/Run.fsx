open System
open System.Diagnostics

let cmd name arg =
    let info = ProcessStartInfo(name, arg)
    info.RedirectStandardOutput <- true
    info.UseShellExecute <- false
    info.CreateNoWindow <- true
    let proc = Process.Start(info)
    proc.OutputDataReceived.Add(fun x -> Console.WriteLine(x.Data))
    proc.BeginOutputReadLine()
    proc.WaitForExit()
    proc.ExitCode

cmd "dotnet" "run -p ..\..\FsLexLite -- --unicode Lexer.fsl --lexlib Microsoft.FSharp.Text.Lexing"
cmd "dotnet" "run -p ..\..\FsYaccLite -- --module Test.Parser --lexlib Microsoft.FSharp.Text.Lexing --parslib Microsoft.FSharp.Text.Parsing -v --newprec Parser.fsy"
cmd "dotnet" "run"