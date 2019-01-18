signature PONYO_INT =
sig
    type t = int

    val compare : int -> int -> order
    val fromString : string -> int option
    val hash : int -> Word64.word
    val toString : int -> string
    val uninitialized : int
end