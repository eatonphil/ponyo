signature MAP =
sig
    eqtype t list
    val get 
end

structure Map =
struct
    signature Ord =
    sig
        eqtype t
        val toString : t -> string
	val fromString : string -> t
    end

    functor Make (O: Ord) : MAP =
    struct

    end
end
