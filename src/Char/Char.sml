structure Ponyo_Char_internal =
struct
    type t = char
    open Char

    fun compare (l: char) (r: char) =
        Char.compare (l, r)

    fun hash (c: char) =
        Word64.fromInt (ord c)

    val unitialized = chr (0)
end
