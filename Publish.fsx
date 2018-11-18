open System
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
    Directory.CreateDirectory(dirTo) |> ignore
    File.Copy(pathFrom, Path.Combine(dirTo, Path.GetFileName(pathFrom)), true)

let main() =
    try
        if Directory.Exists("Publish") then
            Directory.Delete("Publish", true)

        cmd "dotnet" @"publish -c Release FsLexLite\FsLexLite.fsproj -o ..\Publish\FsLexLite"
        cmd "dotnet" @"publish -c Release FsYaccLite\FsYaccLite.fsproj -o ..\Publish\FsYaccLite"

        cp @"Runtime\Lexing.fsi"  @"Publish\Runtime"
        cp @"Runtime\Lexing.fs"   @"Publish\Runtime"
        cp @"Runtime\Parsing.fsi" @"Publish\Runtime"
        cp @"Runtime\Parsing.fs"  @"Publish\Runtime"
    with
        Failure msg -> Console.Error.WriteLine(msg)

main()