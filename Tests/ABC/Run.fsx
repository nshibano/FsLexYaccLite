﻿open System
open System.Diagnostics
open System.IO

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

let cp pathFrom dirTo =
    File.Copy(pathFrom, Path.Combine(dirTo, Path.GetFileName(pathFrom)), true)

let main() =
    try
        cmd "dotnet" @"build ..\..\Arg"
        cmd "dotnet" @"build ..\..\FsLexLite"
        cmd "dotnet" @"build ..\..\FsYaccLite"

        cmd "dotnet" @"..\..\FsYaccLite\bin\Debug\netcoreapp2.1\FsYaccLite.dll --module Test.Parser --lexlib Microsoft.FSharp.Text.Lexing --parslib Microsoft.FSharp.Text.Parsing -v --newprec Parser.fsy"
    with
        Failure msg -> Console.Error.WriteLine(msg)

main()