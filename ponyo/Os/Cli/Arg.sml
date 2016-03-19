structure Arg =
struct
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
        let
            fun contains (l, v) = List.exists (fn (v') => v' = v) l
        in
            Basic ("bool", (fn (arg) =>
                contains (["true", "false"], String.toLower arg)))
        end

    fun optional (t: t, default: string) : t =
        case t of
            Optional t => raise Fail "Cannot have optional optional argument."
            | t => Optional (t, default)

    fun list (t: t) : t =
        List t
end
