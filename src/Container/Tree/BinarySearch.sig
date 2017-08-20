signature PONYO_CONTAINER_TREE_BINARYSEARCH =
sig
    type elt
    type 'a t

    val new : 'a t
    val insert : 'a t -> (elt * 'a) -> 'a t
    val get : 'a t -> elt -> 'a option
    val toList : 'a t -> (elt * 'a) list
    val fromList : (elt * 'a) list -> 'a t
end
