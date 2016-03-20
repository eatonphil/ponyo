structure Ponyo_Os_Cli_Flag :
sig
    type anon
    type named
    datatype t = A of anon | N of named

    val Anon : string -> anon
    val Named : string * string -> named
    val match : t * string -> bool
end =
struct
    type anon = string
    type named = string * string

    datatype t = A of anon | N of named

    fun Anon (name) : anon = name
    fun Named (short, long) : named = (short, long)

    fun match (flag: t, arg: string) : bool =
        case flag of
            N (shortFlag, longFlag) =>
                arg = "-" ^ shortFlag orelse arg = "--" ^ longFlag
          | A (name) => name = arg
end
