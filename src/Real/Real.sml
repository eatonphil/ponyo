structure Ponyo_Real : PONYO_REAL =
struct
    type t = real

    open Real

    fun compare (l: real) (r: real) : order =
        Real.compare (l, r)

    val uninitialized = 0.0
end
