signature PONYO_REAL =
sig
    type t = real

    val compare : real -> real -> order
    val uninitialized : real
    val toString : real -> string
end