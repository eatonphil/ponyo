structure Ponyo_Os_Cli =
struct
    local
        structure Format = Ponyo_Format
        structure String = Ponyo_String
    in

    structure Flag = Ponyo_Os_Cli_Flag
    structure Arg = Ponyo_Os_Cli_Arg

    type anonSpec = Flag.anon * Arg.t * string
    type namedSpec = Flag.named * Arg.t * string

    type cliSpec = string * string * anonSpec list * namedSpec list

    fun doHelp ((name, desc, anonSpecs, namedSpecs): cliSpec) : unit =
        let
            fun genAnonHelp (specs: anonSpec list, usage: (string * string) list)
                    : (string * string) list =
                case specs of
                    [] => List.rev usage
                  | spec as (name, argType, desc) :: specs =>
                genAnonHelp (specs, (case argType of
                    Arg.Optional (_, default) =>
                        (Format.sprintf "[%=\"%\"]" [name, default], name ^ "\t" ^ desc)
                  | Arg.Basic _ => (name, name ^ "\t" ^ desc)
                  | _ => raise Fail "Anonymous argument must not be list.") :: usage)

            val (anonSpecHelp, anonSpecUsage) =
                ListPair.unzip (genAnonHelp (anonSpecs, []))
            val (anonSpecHelp, anonSpecUsage) =
                (String.join (anonSpecHelp, " "), String.join (anonSpecUsage, "\n\t"))

            fun genNamedHelp (specs: namedSpec list, usage: (string * string) list)
                    : (string * string) list =
                case specs of
                    [] => List.rev usage
                  | spec as (name as (short, long), argType, desc) :: specs =>
                genNamedHelp (specs, (case argType of
                    Arg.Optional (_, default) =>
                        (if default <> "" andalso default <> "false"
                            then Format.sprintf "[-%=\"%\"]" [short, default]
                         else Format.sprintf "[-%]" [short],
                         Format.sprintf "-%, --%\t%" [short, long, desc])
                  | Arg.Basic _ =>
                        (Format.sprintf "[-%]" [short],
                         Format.sprintf "-%, --%\t%" [short, long, desc])
                  | Arg.List (Arg.Basic (_)) =>
                        (Format.sprintf "[-%]..." [short],
                         Format.sprintf "-%, --%\t%" [short, long, desc])
                  | _ => raise Fail "Named argument error.") :: usage)

            val (namedSpecHelp, namedSpecUsage) =
                ListPair.unzip (genNamedHelp (namedSpecs, []))
            val (namedSpecHelp, namedSpecUsage) =
                (String.join (namedSpecHelp, " "), String.join (namedSpecUsage, "\n\t"))

            fun helpString () : string =
                let
                    val hs = ref (Format.sprintf "Usage:\n\n\t %" [name])
                in
                    hs := (if anonSpecHelp <> "" then !hs ^ " " ^ anonSpecHelp else !hs);
                    hs := (if namedSpecHelp <> "" then !hs ^ " " ^ namedSpecHelp else !hs);
                    hs := (!hs ^ "\n");
                    hs := (if desc <> "" then desc ^ "\n\n" ^ !hs else !hs);
                    hs := (if anonSpecUsage <> "" orelse namedSpecUsage <> ""
                           then !hs ^ "\nOptions:\n" else !hs);
                    hs := (if anonSpecUsage <> ""
                           then !hs ^ "\n\t" ^ anonSpecUsage else !hs);
                    hs := (if namedSpecUsage <> ""
                           then !hs ^ "\n\t" ^ namedSpecUsage else !hs);
                    !hs ^ "\n"
                end
        in
            Format.printf "%" [helpString ()];
            Basis.OS.Process.exit (Basis.OS.Process.success)
        end

    type args = string * string list

    fun badFlag (name, string) =
        raise Fail (Format.sprintf "Anonymous argument [%] must not be %." [name, string])

    fun badArg (name, typeDesc, arg) =
        raise Fail
            (Format.sprintf "Expected anonymous argument \"%\" to be of type %, got \"%\"."
            [name, typeDesc, arg])

    type getAnonArgsT = string list * args list * anonSpec list
    fun getAnonArgs ((args, parsed, specs) : getAnonArgsT) : getAnonArgsT =
        case args of
            [] => ([], parsed, specs)
          | arg :: args =>
        case String.hasPrefix (arg, "-") of
            true => (arg :: args, parsed, specs)
          | false =>
        case specs of
            [] => (arg :: args, parsed, [])
          | spec as (name, argType, desc) :: specs =>
        case argType of
            Arg.List _ => badFlag (name, "list")
          | Arg.Optional (Arg.List _, _) => badFlag (name, "list")
          | Arg.Optional (Arg.Optional _, _) => badFlag (name, "nested optionals")
          | Arg.Optional (Arg.Basic (typeDesc, conv), _) =>
              if conv (arg)
                  then getAnonArgs (args, (name, [arg]) :: parsed, specs)
              else badArg (name, typeDesc, arg)
          | Arg.Basic (typeDesc, conv) =>
              if conv (arg)
                  then getAnonArgs (args, (name, [arg]) :: parsed, specs)
              else badArg (name, typeDesc, arg)

    type getAnonDefaultsT = anonSpec list * (string * string list) list
    fun getAnonDefaults ((specs, defaults) : getAnonDefaultsT) : getAnonDefaultsT =
        case specs of
            [] => ([], defaults)
          | (spec as (name, argType, desc) : anonSpec) :: specs =>
        case argType of
            Arg.Optional (Arg.Basic (name, conv), default) =>
                getAnonDefaults (specs, (name, [default]) :: defaults)
          | _ =>
              let
                  val (required, defaults) = getAnonDefaults (specs, defaults)
              in
                  (spec :: required, defaults)
              end

    fun parseNamedArgs (args: string list, unmatched: namedSpec list, specs: namedSpec list)
            : string list * namedSpec list * namedSpec list * args =
        case args of
            [] => raise Fail "Expected args."
          | arg :: args =>
        case specs of
            [] => (arg :: args, unmatched, specs, ("", []))
          | (spec as (flag, argType, desc) : namedSpec) :: specs =>
        case Flag.match (Flag.N flag, arg) of
            false => parseNamedArgs (arg :: args, spec :: unmatched, specs)
          | true =>
        let
            fun parseArgs (conv, typeDesc, args, n, parsed) =
                case args of
                    [] => ([], unmatched, specs, (arg, parsed))
                  | nextArg :: args =>
                if String.hasPrefix (nextArg, "-")
                    then (nextArg :: args, unmatched, specs, (arg, parsed))
                else case n of
                    0  => (nextArg :: args, unmatched, specs, (arg, parsed))
                  | n =>
                      if conv (nextArg)
                          then parseArgs (conv, typeDesc, args, n - 1, nextArg :: parsed)
                      else badArg (arg, typeDesc, nextArg)
        in
            case argType of
                Arg.Basic (typeDesc, conv) =>
                  if typeDesc = "bool" then (args, unmatched, specs, (arg, ["true"]))
                  else parseArgs (conv, typeDesc, args, 1, [])
              | Arg.Optional (Arg.Basic (typeDesc, conv), default) =>
                  if typeDesc = "bool" then (args, unmatched, specs, (arg, ["true"]))
                  else (case parseArgs (conv, typeDesc, args, 1, []) of
                      (args, unmatched, specs, (arg, [])) =>
                          (args, unmatched, specs, (arg, [default]))
                    | parsedArgs => parsedArgs)
              | Arg.List (Arg.Basic (typeDesc, conv)) =>
                  parseArgs (conv, typeDesc, args, ~1, [])
              | _ => raise Fail ""
        end

    fun missingRequired (specs: anonSpec list) : unit =
        let
            val missingNames =
                String.join (map (fn (name, _, _) => name) specs, ", ")
        in
            raise Fail
                (Format.sprintf "Missing required arguments [%]." [missingNames])
        end

    type getNamedArgsT = string list * args list * namedSpec list
    fun getNamedArgs ((args, parsed, specs) : getNamedArgsT) : getNamedArgsT =
        case args of
            [] => ([], parsed, specs)
          | (arg: string) :: args =>
        case String.hasPrefix (arg, "-") of
            false => (arg :: args, parsed, specs)
          | true =>
        case specs of
            [] => (arg :: args, parsed, [])
          | _ =>
        case parseNamedArgs (arg :: args, [], specs) of
            (_, _, _, ("", _)) =>
               raise Fail
                   (Format.sprintf "No such flag \"%\" defined." [arg])
          | (args, unmatched, specs, parsedArg) =>
               getNamedArgs (args, parsedArg :: parsed, unmatched @ specs)

    fun getNamedDefaults (specs: namedSpec list, defaults: args list) : args list =
        case specs of
            [] => defaults
          | ((name, _), argType, desc) :: specs =>
        case argType of
            Arg.Optional (Arg.Basic _, default) =>
              getNamedDefaults (specs, ("-" ^ name, [default]) :: defaults)
          | Arg.Optional (Arg.List _, default) =>
              getNamedDefaults (specs, ("-" ^ name, [default]) :: defaults)
          | _ => getNamedDefaults (specs, defaults)

    fun getArgs (cliSpec as (_, _, anonSpecs, namedSpecs): cliSpec) : args list =
        let
            val args = CommandLine.arguments ()

            val (args, anonParsed, remainingSpecs) = getAnonArgs (args, [], anonSpecs)
            val (requiredAnonSpecs, anonDefaults) = getAnonDefaults (remainingSpecs, [])

            val _ = case requiredAnonSpecs of
                [] => ()
              | required => missingRequired (required)

            val (args, namedParsed, namedSpecs) = getNamedArgs (args, [], namedSpecs)
            val namedDefaults = getNamedDefaults (namedSpecs, [])
            val parsed = anonDefaults @ namedDefaults @ namedParsed @ anonParsed
        in
            parsed
        end

    fun getArg (args: args list, flag: Flag.t) : string list =
        case args of
            [] => []
          | arg as (name, vals) :: args =>
        if Flag.match (flag, name) then vals
        else getArg (args, flag)

    fun getAnon (args: args list, flag: Flag.anon) : string list =
        getArg (args, Flag.A flag)

    fun getNamed (args: args list, flag: Flag.named) : string list =
        getArg (args, Flag.N flag)

    end
end
