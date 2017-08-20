structure Ponyo_Sml_Token =
struct
    datatype t =
        String  of string
      | Number  of string
      | Ident   of string
      | Symbol  of string
      | Comment of string

    fun toString (t: t) : string =
        case t of
            String s  => s
          | Number s  => s
          | Ident s   => s
          | Symbol s  => s
          | Comment s => s

    fun compare (a: t, b: t) : order =
        case (a, b) of
            (String a, String b)   => String.compare (a, b)
          | (Number a, Number b)   => String.compare (a, b)
          | (Ident a, Ident b)     => String.compare (a, b)
          | (Symbol a, Symbol b)   => String.compare (a, b)
          | (Comment a, Comment b) => String.compare (a, b)
          | _ => LESS
end
