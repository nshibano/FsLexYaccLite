// (c) Microsoft Corporation 2005-2009. 
/// A simple command-line argument processor.
module FsLexYaccLite.Arg

open System

type ArgType = 
    | ClearArg of bool ref
    | FloatArg of (float -> unit)
    | IntArg of (int -> unit)
    | RestArg of (string -> unit)
    | SetArg of bool ref
    | StringArg of (string -> unit)
    | UnitArg of (unit -> unit)

type ArgInfo =
    { Name : string
      ArgType : ArgType
      HelpText : string }
  
exception InvalidCommandArgument of string option

let parseCommandLineArgs (specs : ArgInfo list) (other : string -> unit) (usageText : string) = 
    let argv = System.Environment.GetCommandLineArgs() 
    let mutable pos = 1
    try
        while pos < argv.Length do
            let arg = argv.[pos] 
            let rec findMatchingArg (specs : ArgInfo list) = 
                match specs with
                | spec :: tl ->
                    if spec.Name = arg then

                        let getSecondArg() = 
                            if pos + 1 >= argv.Length then 
                                raise(InvalidCommandArgument(Some (sprintf "option %s needs an argument." spec.Name)))
                            argv.[pos + 1] 
                 
                        match spec.ArgType with 
                        | UnitArg f -> 
                            f()
                            pos <- pos + 1
                        | SetArg f ->
                            f := true
                            pos <- pos + 1
                        | ClearArg f -> 
                            f := false
                            pos <- pos + 1
                        | StringArg f -> 
                            let arg2 = getSecondArg() 
                            f arg2
                            pos <- pos + 2
                        | IntArg f -> 
                            let arg2 = getSecondArg () 
                            let arg2 = try int32 arg2 with _ -> raise(InvalidCommandArgument None) 
                            f arg2
                            pos <- pos + 2
                        | FloatArg f -> 
                            let arg2 = getSecondArg() 
                            let arg2 = try float arg2 with _ -> raise(InvalidCommandArgument None)
                            f arg2
                            pos <- pos + 2
                        | RestArg f -> 
                            pos <- pos + 1
                            while pos < argv.Length do
                                f argv.[pos]
                                pos <- pos + 1
                    else
                        findMatchingArg tl
                | [] -> 
                    if arg = "-help" || arg = "--help" || arg = "/help" || arg = "/help" || arg = "/?" then
                        raise (InvalidCommandArgument None)
                    // Note: for '/abc/def' does not count as an argument
                    // Note: '/abc' does
                    elif arg.Length > 0 && (arg.[0] = '-' || (arg.[0] = '/' && not (arg.Length > 1 && arg.Substring(1).Contains("/")))) then
                        raise (InvalidCommandArgument (Some ("unrecognized argument: " + arg)))
                    else 
                       other arg
                       pos <- pos + 1
            findMatchingArg specs 
    with InvalidCommandArgument msg ->
        let f = Console.Error
        Option.iter (fun (msg : string) -> f.WriteLine(msg)) msg
        f.WriteLine(usageText)
        for info in specs do
            match info.ArgType with
            | UnitArg _ | SetArg _ | ClearArg _ -> fprintfn f "\t%s: %s" info.Name info.HelpText
            | StringArg _ -> fprintfn f "\t%s <string>: %s" info.Name info.HelpText
            | IntArg _ -> fprintfn f "\t%s <int>: %s" info.Name info.HelpText
            | FloatArg _ -> fprintfn f "\t%s <float>: %s" info.Name info.HelpText
            | RestArg _ -> fprintfn f "\t%s ...: %s" info.Name info.HelpText
        fprintfn f "\t--help: display this list of options"
        fprintfn f "\t-help: display this list of options"
        f.Flush()
        Environment.Exit(1)