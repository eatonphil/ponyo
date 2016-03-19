structure Token =
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
end
