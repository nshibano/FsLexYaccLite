open System
open System.Diagnostics

let cmd name arg =
    let info = ProcessStartInfo(name, arg)
    info.RedirectStandardOutput <- true
    info.RedirectStandardError <- true
    info.UseShellExecute <- false
    info.CreateNoWindow <- true
    let proc = Process.Start(info)
    proc.OutputDataReceived.Add(fun x -> Console.WriteLine(x.Data))
    proc.ErrorDataReceived.Add(fun x -> Console.Error.WriteLine(x.Data))
    proc.BeginOutputReadLine()
    proc.BeginErrorReadLine()
    proc.WaitForExit()
    if proc.ExitCode <> 0 then failwithf "command %s %s failed with exit code %d." name arg proc.ExitCode

let main() =
    try
        cmd "dotnet" @"build ..\..\FsLexLite"
        cmd "dotnet" @"build ..\..\FsYaccLite"

        cmd "dotnet" @"..\..\FsLexLite\bin\Debug\netcoreapp2.1\FsLexLite.dll --unicode Lexer.fsl --lexlib Microsoft.FSharp.Text.Lexing"
        cmd "dotnet" @"..\..\FsYaccLite\bin\Debug\netcoreapp2.1\FsYaccLite.dll --module Test.Parser --lexlib Microsoft.FSharp.Text.Lexing --parslib Microsoft.FSharp.Text.Parsing -v --newprec Parser.fsy"

        cmd "dotnet" @"run"
    with
        Failure msg -> Console.Error.WriteLine(msg)

main()