signature PONYO_SML_TOKEN =
sig
    datatype token =
        String  of string
      | Number  of string
      | Ident   of string
      | Symbol  of string
      | Comment of string

    type t = { token: token, line: int, col: int }

    val toString : t -> string
    val compare : t -> t -> order
end
