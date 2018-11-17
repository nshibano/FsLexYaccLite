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

let getUsage specs u =
    let sbuf = new System.Text.StringBuilder 100  
    let pstring (s:string) = sbuf.Append s |> ignore 
    let pendline s = pstring s; pstring "\n" 
    pendline u;
    List.iter (fun (arg:ArgInfo) -> 
    match arg.Name, arg.ArgType, arg.HelpText with
    | (s, (UnitArg _ | SetArg _ | ClearArg _), helpText) -> pstring "\t"; pstring s; pstring ": "; pendline helpText
    | (s, StringArg _, helpText) -> pstring "\t"; pstring s; pstring " <string>: "; pendline helpText
    | (s, IntArg _, helpText) -> pstring "\t"; pstring s; pstring " <int>: "; pendline helpText
    | (s, FloatArg _, helpText) ->  pstring "\t"; pstring s; pstring " <float>: "; pendline helpText
    | (s, RestArg _, helpText) -> pstring "\t"; pstring s; pstring " ...: "; pendline helpText) specs;
    pstring "\t"; pstring "--help"; pstring ": "; pendline "display this list of options";
    pstring "\t"; pstring "-help"; pstring ": "; pendline "display this list of options";
    sbuf.ToString()

let parseCommandLineArgs (specs : ArgInfo list) (other : string -> unit) (usageText : string) = 
        let current = ref 0
        let argv = System.Environment.GetCommandLineArgs() 
        try
            let nargs = Array.length argv 
            incr current;
            let argSpecs = specs |> Seq.toList
            let specs = argSpecs |> List.map (fun (arg:ArgInfo) -> arg.Name, arg.ArgType)
            while !current < nargs do
              let arg = argv.[!current] 
              let rec findMatchingArg args = 
                match args with
                | ((s, action) :: _) when s = arg -> 
                   let getSecondArg () = 
                       if !current + 1 >= nargs then 
                         raise(InvalidCommandArgument(Some ("option "+s+" needs an argument.")));
                       argv.[!current+1] 
                 
                   match action with 
                   | UnitArg f -> 
                     f (); 
                     incr current
                   | SetArg f ->
                     f := true; 
                     incr current
                   | ClearArg f -> 
                     f := false; 
                     incr current
                   | StringArg f-> 
                     let arg2 = getSecondArg() 
                     f arg2; 
                     current := !current + 2
                   | IntArg f -> 
                     let arg2 = getSecondArg () 
                     let arg2 = try int32 arg2 with _ -> raise(InvalidCommandArgument None) in  
                     f arg2;
                     current := !current + 2;
                   | FloatArg f -> 
                     let arg2 = getSecondArg() 
                     let arg2 = try float arg2 with _ -> raise(InvalidCommandArgument None) in 
                     f arg2; 
                     current := !current + 2;
                   | RestArg f -> 
                     incr current;
                     while !current < nargs do
                         f (argv.[!current]);
                         incr current;

                | (_ :: more)  -> findMatchingArg more 
                | [] -> 
                    if arg = "-help" || arg = "--help" || arg = "/help" || arg = "/help" || arg = "/?" then
                        raise (InvalidCommandArgument None)
                    // Note: for '/abc/def' does not count as an argument
                    // Note: '/abc' does
                    elif arg.Length>0 && (arg.[0] = '-' || (arg.[0] = '/' && not (arg.Length > 1 && arg.[1..].Contains ("/")))) then
                        raise (InvalidCommandArgument (Some ("unrecognized argument: "+ arg)))
                    else 
                       other arg;
                       incr current
              findMatchingArg specs 
        with InvalidCommandArgument msg -> 
              Console.Error.WriteLine(msg)
              match msg with
              | Some msg -> Console.Error.WriteLine(getUsage specs usageText)
              | None -> ()
              Console.Error.Flush()
              Environment.Exit(1)
