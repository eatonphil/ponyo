(*
 *  PONYO_ENCODING_JSON: This is a JSON library.
 *
 *  Ex:
 *      local
 *          open Ponyo.Encoding.Json
 *          infix >>=
 *          val json = "{\"a\": 1, \"b\": [3, 4]}"
 *      in
 *          { a = 1, b = [3, 4] } = Ponyo.Encoding.Json.decode (json) >>=
 *      end
 *)
signature PONYO_ENCODING_JSON =
sig
    type t

    val >>= : 'a * ('b -> 'a -> 'a) -> 'a

    (*
     *  decode: Takes a JSON string and returns an internal object
     *  that can be marshaled into a Standard ML struct.
     *)
    val decode : string -> t
    val encode : t -> string
    val equals : t -> t -> bool

    val marshal : (t * string) -> (t * t) option
    val marshalString : (t * string) -> (t * string) option
    val marshalInt : (t * string) -> (t * int) option
    val marshalList : (t * string) -> (t * t list) option
    val marshalReal : (t * string) -> (t * real) option
    val marshalBool : (t * string) -> (t * bool) option
end