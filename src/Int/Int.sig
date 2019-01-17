signature PONYO_INT =
sig
    type t = int

    val compare : int -> int -> order
    val hash : int -> Word64.word
    val uninitialized : int
    val toString : int -> string
end