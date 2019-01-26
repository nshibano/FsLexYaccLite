#load "Common.fsx"
#r @"System.IO.Compression.dll"
#r @"System.IO.Compression.FileSystem.dll"

open System
open System.IO
open System.IO.Compression

open Common

let inAppveyor, buildVersion =
    let s = Environment.GetEnvironmentVariable("APPVEYOR_BUILD_VERSION")
    if isNull s then
        false, "0.0.0"
    else
        true, s

let pushArtifact path =
    if inAppveyor then
        cmd "appveyor" ("PushArtifact " + path)
    
try
    if Directory.Exists("Publish") then
        Directory.Delete("Publish", true)

    cmd "dotnet" @"publish -c Release FsLexLite\FsLexLite.fsproj -o ..\Publish\core21\FsLexLite"
    cmd "dotnet" @"publish -c Release FsYaccLite\FsYaccLite.fsproj -o ..\Publish\core21\FsYaccLite"
    cp @"Runtime\FsLexYaccLiteRuntime.fs"   @"Publish\core21\Runtime"
    let core21ZipFileName = sprintf "FsLexYaccLite-core21-%s.zip" buildVersion
    if File.Exists(core21ZipFileName) then
        File.Delete(core21ZipFileName)
    ZipFile.CreateFromDirectory(@"Publish\core21", core21ZipFileName)
    pushArtifact core21ZipFileName

    cp @"Common\Arg.fs" @"Net45\Common"
    cp @"Common\Hashtable.fs" @"Net45\Common"
    cp @"Common\Output.fs" @"Net45\Common"

    let lexFiles = Array.concat [| Directory.GetFiles("FsLexLite", "*.fs"); Directory.GetFiles("FsLexLite", "*.fsi") |]
    for lexFile in lexFiles do
        cp lexFile @"Net45\FsLexLite"
    let yaccFiles = Array.concat [| Directory.GetFiles("FsYaccLite", "*.fs"); Directory.GetFiles("FsYaccLite", "*.fsi") |]
    for yaccFile in yaccFiles do
        cp yaccFile @"Net45\FsYaccLite"
    cmd "msbuild" @"/p:Configuration=Release Net45\Net45.sln"
    Directory.CreateDirectory(@"Publish\net45\Bin") |> ignore
    cpAllFiles @"Net45\FsLexLite\bin\Release" @"Publish\net45\Bin" true
    cpAllFiles @"Net45\FsYaccLite\bin\Release" @"Publish\net45\Bin" true
    cp @"Runtime\FsLexYaccLiteRuntime.fs"   @"Publish\net45\Runtime"
    let net45ZipFileName = sprintf "FsLexYaccLite-net45-%s.zip" buildVersion
    if File.Exists(net45ZipFileName) then
        File.Delete(net45ZipFileName)
    ZipFile.CreateFromDirectory(@"Publish\net45", net45ZipFileName)
    pushArtifact net45ZipFileName
with
    Failure msg -> Console.Error.WriteLine(msg)