// (c) Microsoft Corporation 2005-2009. 
/// A simple command-line argument processor.

namespace Microsoft.FSharp.Text

/// The spec value describes the action of the argument,
/// and whether it expects a following parameter.
type ArgType = 
  | ClearArg of bool ref
  | FloatArg of (float -> unit)
  | IntArg of (int -> unit)
  | RestArg of (string -> unit)
  | SetArg of bool ref
  | StringArg of (string -> unit)
  | UnitArg of (unit -> unit)

type ArgInfo (name,action,help) = 
  /// Return the name of the argument
  member x.Name = name
  /// Return the argument type and action of the argument
  member x.ArgType = action
  /// Return the usage help associated with the argument
  member x.HelpText = help
  
exception Bad of string
exception HelpText of string

[<Sealed>]
type ArgParser() = 
    static let getUsage specs u =  
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
        | (s, RestArg _, helpText) -> pstring "\t"; pstring s; pstring " ...: "; pendline helpText)
        specs;
      pstring "\t"; pstring "--help"; pstring ": "; pendline "display this list of options";
      pstring "\t"; pstring "-help"; pstring ": "; pendline "display this list of options";
      sbuf.ToString()

    /// Parse some of the arguments given by 'argv', starting at the given position
    [<System.Obsolete("This method should not be used directly as it will be removed in a future revision of this library")>]
    static member ParsePartial(cursor,argv,argSpecs:seq<ArgInfo>,?other,?usageText) =
        let other = defaultArg other (fun _ -> ())
        let usageText = defaultArg usageText ""
        let nargs = Array.length argv 
        incr cursor;
        let argSpecs = argSpecs |> Seq.toList
        let specs = argSpecs |> List.map (fun (arg:ArgInfo) -> arg.Name, arg.ArgType)
        while !cursor < nargs do
          let arg = argv.[!cursor] 
          let rec findMatchingArg args = 
            match args with
            | ((s, action) :: _) when s = arg -> 
               let getSecondArg () = 
                   if !cursor + 1 >= nargs then 
                     raise(Bad("option "+s+" needs an argument.\n"+getUsage argSpecs usageText));
                   argv.[!cursor+1] 
                 
               match action with 
               | UnitArg f -> 
                 f (); 
                 incr cursor
               | SetArg f ->
                 f := true; 
                 incr cursor
               | ClearArg f -> 
                 f := false; 
                 incr cursor
               | StringArg f-> 
                 let arg2 = getSecondArg() 
                 f arg2; 
                 cursor := !cursor + 2
               | IntArg f -> 
                 let arg2 = getSecondArg () 
                 let arg2 = try int32 arg2 with _ -> raise(Bad(getUsage argSpecs usageText)) in  
                 f arg2;
                 cursor := !cursor + 2;
               | FloatArg f -> 
                 let arg2 = getSecondArg() 
                 let arg2 = try float arg2 with _ -> raise(Bad(getUsage argSpecs usageText)) in 
                 f arg2; 
                 cursor := !cursor + 2;
               | RestArg f -> 
                 incr cursor;
                 while !cursor < nargs do
                     f (argv.[!cursor]);
                     incr cursor;

            | (_ :: more)  -> findMatchingArg more 
            | [] -> 
                if arg = "-help" || arg = "--help" || arg = "/help" || arg = "/help" || arg = "/?" then
                    raise (HelpText (getUsage argSpecs usageText))
                // Note: for '/abc/def' does not count as an argument
                // Note: '/abc' does
                elif arg.Length>0 && (arg.[0] = '-' || (arg.[0] = '/' && not (arg.Length > 1 && arg.[1..].Contains ("/")))) then
                    raise (Bad ("unrecognized argument: "+ arg + "\n" + getUsage argSpecs usageText))
                else 
                   other arg;
                   incr cursor
          findMatchingArg specs 
    /// Prints the help for each argument.
    static member Usage (specs,?usage) = 
        let usage = defaultArg usage ""
        System.Console.Error.WriteLine (getUsage (Seq.toList specs) usage)

    /// Parse the arguments given by System.Environment.GetEnvironmentVariables()
    /// according to the argument processing specifications "specs".
    /// Args begin with "-". Non-arguments are passed to "f" in
    /// order.  "use" is printed as part of the usage line if an error occurs.
    static member Parse (specs,?other,?usageText) = 
        let current = ref 0
        let argv = System.Environment.GetCommandLineArgs() 
        try ArgParser.ParsePartial (current, argv, specs, ?other=other, ?usageText=usageText)
        with 
          | Bad h 
          | HelpText h -> 
              System.Console.Error.WriteLine h; 
              System.Console.Error.Flush();  
              System.Environment.Exit(1); 
          | e -> 
              reraise()
