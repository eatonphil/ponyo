signature PONYO_CONTAINER_DICT =
sig
    type elt
    type 'a t
    val newWithSize: int -> 'a t
    val new : unit -> 'a t
    val insert : 'a t -> elt -> 'a -> 'a t
    val get : 'a t -> elt -> 'a option
    val toList : 'a t -> (elt * 'a) list
end
