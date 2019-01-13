#load "Common.fsx"
#r @"System.IO.Compression.dll"
#r @"System.IO.Compression.FileSystem.dll"

open System
open System.IO
open System.IO.Compression

open Common

let buildVersion =
    let s = Environment.GetEnvironmentVariable("APPVEYOR_BUILD_VERSION")
    if isNull s then "0.0.0" else s

let zipFileName = sprintf "FsLexYaccLite-%s.zip" buildVersion

try
    if Directory.Exists("Publish") then
        Directory.Delete("Publish", true)

    cmd "dotnet" @"publish -c Release FsLexLite\FsLexLite.fsproj -o ..\Publish\FsLexLite"
    cmd "dotnet" @"publish -c Release FsYaccLite\FsYaccLite.fsproj -o ..\Publish\FsYaccLite"

    cp @"Runtime\Lexing.fs"   @"Publish\Runtime"
    cp @"Runtime\Parsing.fs"  @"Publish\Runtime"

    if File.Exists(zipFileName) then
        File.Delete(zipFileName)
    ZipFile.CreateFromDirectory("Publish", zipFileName)

    try cmd "appveyor" ("PushArtifact " + zipFileName)
    with _ -> printfn "failed to run appveyor.exe"
with
    Failure msg -> Console.Error.WriteLine(msg)