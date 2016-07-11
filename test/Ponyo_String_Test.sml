structure Ponyo_String_Test =
struct

local
    structure String = Ponyo.String
    val test = Ponyo.Test.test
in

fun run () = [
    test "String.capitalize" let
        val c = String.capitalize
    in [
        c "foobar" = "Foobar",
        c "fOOBAR" = "Foobar",
        c "foobar foobar" = "Foobar foobar",
        c "foobar" = "foobar"
    ] end,

    test "String.charAt" let
        val c = String.charAt
    in [
        c ("foobar", 0) = #"f",
        c ("foobar", ~1) = #"r",
        c ("foobar", 6) = #"f" handle String.IndexError _ => true | e => (PolyML.print e; false)
    ] end
]

end
end
