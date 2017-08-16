structure Ponyo_Int =
struct
    type t = int

    open Int

    val hash = fn (i) => Word64.fromInt (i)
    val unitialized = 0
end
