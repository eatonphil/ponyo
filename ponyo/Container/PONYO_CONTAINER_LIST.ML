signature PONYO_CONTAINER_LIST =
sig
    type element

    include LIST

    val count : element list * element -> int
    val contains : element list * element -> bool
    val get : element list * int -> element option
    val sort : element list -> element list
    val sublist : element list * int * int -> element list
    val sublistToEnd : element list * int -> element list
end

signature PONYO_CONTAINER_LIST_DOMAIN =
sig
    eqtype t

    val compare : t * t -> order
end
