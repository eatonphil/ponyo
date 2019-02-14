structure Ponyo_Sml_Token : PONYO_SML_TOKEN =
struct
    datatype token =
        String  of string
      | Number  of string
      | Ident   of string
      | Symbol  of string
      | Comment of string

    type t = { token: token, line: int, col: int }

    fun format (s, col, line) = s ^ " at " ^ (Int.toString line) ^ ", " ^ (Int.toString col)

    fun toString (t as { token, col, line }: t) : string =
        case token of
            String s => format (s, col, line)
          | Number n => format (n, col, line)
          | Ident i => format (i, col, line)
          | Symbol s => format (s, col, line)
          | Comment c => format (c, col, line)

    fun compare ({ token = String a, ... }) ({ token = String b, ... }) = String.compare (a, b)
      | compare ({ token = Number a, ... }) ({ token = Number b, ... }) = String.compare (a, b)
      | compare ({ token = Ident a, ... }) ({ token = Ident b, ... }) = String.compare (a, b)
      | compare ({ token = Symbol a, ... }) ({ token = Symbol b, ... }) = String.compare (a, b)
      | compare ({ token = Comment a, ... }) ({ token = Comment b, ... }) = String.compare (a, b)
      | compare _ _ = LESS
end
