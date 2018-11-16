open System.IO

let files =
    [
        "Arg.fsi"
        "Arg.fs"
        "Lexing.fsi"
        "Lexing.fs"
        "Parsing.fsi"
        "Parsing.fs"
    ]

let dirs = 
    [
        "FsLexLite"
        "FsYaccLite"
    ]

for file in files do
    for dir in dirs do
        let pathFrom = Path.Combine("Common", file)
        let pathTo = Path.Combine(dir, file)
        File.Copy(pathFrom, pathTo, true)