(*
 *  PONYO_CONTAINER_DICT: This is a hash table.
 *
 *  Ex:
 *      local
 *	    open Ponyo.String
 *      in
 *          val config = Dict.new
 *          val config = Dict.insert config "PRODUCTION" 1
 *          val config = Dict.insert config "DEVELOPMENT" 2
 *          val develLevel = Dict.get config  "DEVELOPMENT"
 *	end
 *)
signature PONYO_CONTAINER_DICT =
sig
    type elt
    type 'a t
    val newWithSize: int -> 'a t
    val new : unit -> 'a t
    val insert : 'a t -> elt -> 'a -> 'a t
    val get : 'a t -> elt -> 'a option
    val toList : 'a t -> (elt * 'a) list
    val fromList : (elt * 'a) list -> 'a t
end
