﻿namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsLex")>]
[<assembly: AssemblyProductAttribute("FsLex")>]
[<assembly: AssemblyDescriptionAttribute("FsLex/FsYacc lexer/parser generation tools")>]
[<assembly: AssemblyVersionAttribute("6.0.0")>]
[<assembly: AssemblyFileVersionAttribute("6.0.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "6.0.0"