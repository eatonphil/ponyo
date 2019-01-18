signature PONYO_REAL =
sig
    type t = real

    val compare : real -> real -> order
    val fromString : string -> real option
    val toString : real -> string
    val uninitialized : real
end