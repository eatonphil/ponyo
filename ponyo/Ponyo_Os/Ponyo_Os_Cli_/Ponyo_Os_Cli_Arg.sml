structure Ponyo_Os_Cli_Arg =
struct
    local
        structure String = Ponyo_String
        structure StringList = Ponyo_Container_List (String)
    in

    datatype t =
        Basic of string * (string -> bool)
        | Optional of t * string
        | List of t

    val string : t = Basic ("string", fn (arg) => true)

    val int : t =
        Basic ("int", (fn (arg) => isSome (Int.fromString arg)))

    val real : t =
        Basic ("real", (fn (arg) => isSome (Real.fromString arg)))

    val bool : t =
        Basic ("bool", (fn (arg) =>
            StringList.contains (["true", "false"], String.toLower arg)))

    fun optional (t: t, default: string) : t =
        case t of
            Optional t => raise Fail "Cannot have optional optional argument."
            | t => Optional (t, default)

    fun list (t: t) : t =
        List t

    end
end
