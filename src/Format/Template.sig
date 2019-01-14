(*
 *  PONYO_FORMAT_TEMPLATE: This is a string template library.
 *
 *  Ex:
 *      local
 *        open Ponyo
 *      in
 *        val parsed = Format.Template.parse "{{ a }} = {{ b }}";
 *        val map = String.Dict.new;
 *        val map = String.Dict.insert map "a" 1;
 *        val map = String.Dict.insert map "b" 1;
 *        val result = Format.Template.generate parsed map;
 *      end
 *)
signature PONYO_FORMAT_TEMPLATE =
sig
    type t

    (*
     *  parse: This parses a template into an internal representation.
     *)
    val parse: string -> (t * string)

    (*
     *  generate: This takes an internal template representation and a map
     *  and produces a string.
     *)
    val generate: t -> string Ponyo_String.Dict.t -> string
end
