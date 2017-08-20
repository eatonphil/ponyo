structure Ponyo_Char_internal =
struct
    type t = char
    open Char

    val hash = fn (i) => Word64.fromInt (ord i)
    val unitialized = chr (0)
end
