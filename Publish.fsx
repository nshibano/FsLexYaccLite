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

try
    if Directory.Exists("Publish") then
        Directory.Delete("Publish", true)

    cmd "dotnet" @"publish -c Release FsLexLite\FsLexLite.fsproj -o ..\Publish\core21\FsLexLite"
    cmd "dotnet" @"publish -c Release FsYaccLite\FsYaccLite.fsproj -o ..\Publish\core21\FsYaccLite"
    cp @"Runtime\Lexing.fs"   @"Publish\core21\Runtime"
    cp @"Runtime\Parsing.fs"  @"Publish\core21\Runtime"
    
    let core21ZipFileName = sprintf "FsLexYaccLite-core21-%s.zip" buildVersion
    if File.Exists(core21ZipFileName) then
        File.Delete(core21ZipFileName)
    ZipFile.CreateFromDirectory("Publish\core21", core21ZipFileName)
    try cmd "appveyor" ("PushArtifact " + core21ZipFileName)
    with _ -> printfn "failed to run appveyor.exe"

    cp @"Common\Arg.fs" @"Net45\Common"
    let lexFiles = Array.concat [| Directory.GetFiles("FsLexLite", "*.fs"); Directory.GetFiles("FsLexLite", "*.fsi") |]
    for lexFile in lexFiles do
        cp lexFile @"Net45\FsLexLite"
    cmd "msbuild" @"Net45\Net45.sln"
        

    ()
with
    Failure msg -> Console.Error.WriteLine(msg)