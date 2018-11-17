// (c) Microsoft Corporation 2005-2009. 
/// A simple command-line argument processor.
module FsLexYaccLite.Arg

open System

type ArgType = 
    | IntArg of (int -> unit)
    | StringArg of (string -> unit)
    | UnitArg of (unit -> unit)
  
exception InvalidCommandLineArg of string option

let parseCommandLineArgs (specs : (string * ArgType * string) list) (other : string -> unit) (usageText : string) = 
    let argv = System.Environment.GetCommandLineArgs() 
    let mutable pos = 1
    try
        while pos < argv.Length do
            let arg = argv.[pos] 
            let rec findMatchingArg (specs : (string * ArgType * string) list) = 
                match specs with
                | (name, argType, helpText) :: tl ->
                    if name = arg then

                        let getSecondArg() = 
                            if pos + 1 >= argv.Length then 
                                raise(InvalidCommandLineArg(Some (sprintf "option %s needs an argument." name)))
                            argv.[pos + 1] 
                 
                        match argType with 
                        | UnitArg f -> 
                            f()
                            pos <- pos + 1
                        | StringArg f -> 
                            let arg2 = getSecondArg() 
                            f arg2
                            pos <- pos + 2
                        | IntArg f -> 
                            let arg2 = getSecondArg () 
                            let arg2 = try int32 arg2 with _ -> raise(InvalidCommandLineArg None) 
                            f arg2
                            pos <- pos + 2
                    else
                        findMatchingArg tl
                | [] -> 
                    if arg = "-help" || arg = "--help" || arg = "/help" || arg = "/help" || arg = "/?" then
                        raise (InvalidCommandLineArg None)
                    // Note: for '/abc/def' does not count as an argument
                    // Note: '/abc' does
                    elif arg.Length > 0 && (arg.[0] = '-' || (arg.[0] = '/' && not (arg.Length > 1 && arg.Substring(1).Contains("/")))) then
                        raise (InvalidCommandLineArg (Some ("unrecognized argument: " + arg)))
                    else 
                       other arg
                       pos <- pos + 1
            findMatchingArg specs 
    with InvalidCommandLineArg msg ->
        let f = Console.Error
        Option.iter (fun (msg : string) -> f.WriteLine(msg)) msg
        f.WriteLine(usageText)
        for name, argType, helpText in specs do
            match argType with
            | UnitArg _ -> fprintfn f "\t%s: %s" name helpText
            | StringArg _ -> fprintfn f "\t%s <string>: %s" name helpText
            | IntArg _ -> fprintfn f "\t%s <int>: %s" name helpText
        fprintfn f "\t--help: display this list of options"
        fprintfn f "\t-help: display this list of options"
        f.Flush()
        Environment.Exit(1)