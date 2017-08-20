structure Ponyo_Encoding_Json =
struct
    local
        structure String = Ponyo_String
        structure Format = Ponyo_Format
        structure Char = Ponyo_Char
    in

    structure Token =
    struct
        datatype t =
            String of string
          | Int of int
          | Real of real
          | True
          | False
          | Symbol of string

        val LCBracket = "{"
        val RCBracket = "}"
        val LBracket = "["
        val RBracket = "]"
        val Comma = ","
        val Colon = ":"
    end

    datatype t =
        String of string
      | Int of int
      | Real of real
      | True
      | False
      | List of t list
      | Object of (string * t) list

    fun equals (a: t, b: t) : bool =
        case (a, b) of
            (String aS, String bS) => aS = bS
          | (Int aI, Int bI) => aI = bI
          | (Real aR, Real bR) => false (* TODO: fix this *)
          | (True, True) => true
          | (False, False) => true
          | (List aL, List bL) => List.all (fn a => a = true) (List.map equals (ListPair.zip (aL, bL)))
          | (Object aO, Object bO) => List.all (fn a => a = true) (List.map (fn ((s1, l1), (s2, l2)) => s1 = s2 andalso equals (l1, l2)) (ListPair.zip (aO, bO)))
          | _ => false

    exception MalformedString of char list
    exception MalformedNumber of char list
    exception MalformedTrue
    exception MalformedFalse
    exception MalformedJson of Token.t list * string
    exception MalformedKey of Token.t list
    exception MalformedList of Token.t list

    fun lex (json: char list, l: Token.t list) : Token.t list =
        case json of
            [] => List.rev (l)
          | #"{" :: rest => lex (rest, Token.Symbol Token.LCBracket :: l)
          | #"}" :: rest => lex (rest, Token.Symbol Token.RCBracket :: l)
          | #":" :: rest => lex (rest, Token.Symbol Token.Colon :: l)
          | #"[" :: rest => lex (rest, Token.Symbol Token.LBracket :: l)
          | #"]" :: rest => lex (rest, Token.Symbol Token.RBracket :: l)
          | #"," :: rest => lex (rest, Token.Symbol Token.Comma :: l)
          | #"\"" :: rest => lexString (#"\"", rest, l)
          | #"'" :: rest => lexString (#"'", rest, l)
          | #"t" :: rest => lexTrue (rest, l)
          | #"f" :: rest => lexFalse (rest, l)
          | #" " :: rest => lex (rest, l)
          | #"\r" :: rest => lex (rest, l)
          | #"\t" :: rest => lex (rest, l)
          | #"\n" :: rest => lex (rest, l)
          | _ => lexNumber (json, l)

    and lexString (starter: char, json: char list, l: Token.t list) : Token.t list =
        case json of
            [] => raise MalformedString []
          | _ =>
        let
            fun lexStringHelper (s: char list, accum: char list) : char list * char list =
                case s of
                    [] => raise MalformedString (accum) (* json string ended without closing a string *)
                  | first :: rest =>
                if first = starter
                    then (List.rev accum, rest)
                else if rest = []
                    then raise MalformedString (accum) (* json string ended without closing a string *)
                else if first = #"\\" andalso Char.List.contains ([#"'", #"\""], List.hd rest) (* TODO: \\ *)
                    then lexStringHelper (List.tl rest, List.hd rest :: accum)
                else lexStringHelper (rest, first :: accum)

            val (string, rest) = lexStringHelper (json, [])
            val t = Token.String (String.implode string)
        in
            lex (rest, t :: l)
        end

    and lexNumber (json: char list, l: Token.t list) : Token.t list =
        case json of
            [] => raise MalformedNumber []
          | _ =>
        let
            fun lexNumberHelper (n: char list, accum: char list) : char list * char list =
                case n of
                    [] => (List.rev accum, [])
                  | first :: rest =>
                if Char.List.contains (String.explode("0123456789.-"), first)
                    then lexNumberHelper (rest, first :: accum)
                else (List.rev accum, first :: rest)
            val (n, rest) = lexNumberHelper (json, [])
        
            val t =
                case Int.fromString (String.implode n) of
                    SOME i => Token.Int (i)
                  | _ =>
                case Real.fromString (String.implode n) of
                    SOME r => Token.Real (r)
                  | _ => raise MalformedNumber (n)
        in
            lex (rest, t :: l)
        end

    (* TODO: handle TRUE, True *)
    and lexTrue (json: char list, l: Token.t list) : Token.t list =
        case json of
            #"r" :: (#"u" :: (#"e" :: rest)) => lex (rest, Token.True :: l)
          | _ => raise MalformedTrue

    (* TODO: handle FALSE, False *)
    and lexFalse (json: char list, l: Token.t list) : Token.t list =
        case json of
            #"a" :: (#"l" :: (#"s" :: (#"e" :: rest))) => lex (rest, Token.False :: l)
          | _ => raise MalformedFalse

    infix >>=
    fun a >>= b =
        b (a)

    fun parse (json: string) : t =
        case lex (String.explode json, []) of
            [] => raise MalformedJson ([], "Lexing error")
          | tokens =>
        case parseJson (tokens) of
            (t, _) => t

    and parseJson (json: Token.t list) : t * Token.t list =
        case json of
            Token.Real r :: rest => (Real r, rest)
          | Token.Int r :: rest => (Int r, rest)
          | Token.String s :: rest => (String s, rest)
          | Token.True :: rest => (True, rest)
          | Token.False :: rest => (False, rest)
          | Token.Symbol s :: rest =>
          (if s = Token.LCBracket
              then parseObject (rest)
          else if s = Token.LBracket then parseList (rest) else raise MalformedJson (json, "Expected ending bracket"))
          | _ => raise MalformedJson (json, "Expected json value")

    and parseSymbol (json: Token.t list, s: string) : Token.t list =
        case json of
            Token.Symbol first :: rest => if first = s then rest else []
          | _ => raise MalformedJson (json, "Expected symbol " ^ s)

    and parseString (json: Token.t list) : string * Token.t list =
        case json of
            Token.String first :: rest => (first, rest)
          | _ => raise MalformedKey (json)

    and parseList (json: Token.t list): t * Token.t list =
        parseListElements (json, []) >>= (fn (elements, json) =>
        case parseSymbol (json, Token.RBracket) of
            [] => raise MalformedList (json)
          | json => (List elements, json))

    and parseListElements (json: Token.t list, elements: t list) : t list * Token.t list =
        parseJson (json) >>= (fn (e, json) =>
        case parseSymbol (json, Token.Comma) of
            [] => (List.rev (e :: elements), json)
          | json => parseListElements (json, e :: elements))

    and parseObject (json: Token.t list) : t * Token.t list =
        parsePairs (json, []) >>= (fn (pairs, json) =>
        case parseSymbol (json, Token.RCBracket) of
            [] => (Object (pairs), json)
          | json => (Object (pairs), json))

    and parsePairs (json: Token.t list, pairs: (string * t) list) : (string * t) list * Token.t list =
        parsePair (json) >>= (fn (pair, json) =>
        case parseSymbol (json, Token.Comma) of
            [] => (List.rev (pair :: pairs), json)
          | json => parsePairs (json, pair :: pairs))

    and parsePair (json: Token.t list) : (string * t) * Token.t list =
        parseString (json) >>= (fn (string, json) =>
        case parseSymbol (json, Token.Colon) of
            [] => raise MalformedJson (json, "Expected symbol " ^ Token.Colon)
          | json =>
        parseJson (json) >>= (fn (t, json) =>
        ((string, t), json)))

    structure Marshall =
    struct
        fun a >>= b =
            case a of
                SOME v => b (v)
             | _ => raise Fail "Marshalling error"

        fun marshall (object: t, key: string) : (t * t) option =
            case object of
                Object (pairs) =>
                  let
                      fun findKey (pairs, key) =
                          case pairs of
                              (someKey, someVal) :: pairs => if someKey = key then SOME (object, someVal) else findKey (pairs, key)
                            | [] => NONE
                  in
                      findKey (pairs, key)
                  end
              | _ => (raise Fail "Cannot marshall non-object"; NONE)

        fun marshallString (object: t, key: string) : (t * string) option =
            case marshall (object, key) of
                SOME (object, String v) => SOME (object, v)
              | _ => NONE

        fun marshallInt (object: t, key: string) : (t * int) option =
            case marshall (object, key) of
                SOME (object, Int v) => SOME (object, v)
              | _ => NONE

        fun marshallBool (object: t, key: string) : (t * bool) option =
            case marshall (object, key) of
                SOME (object, True) => SOME (object, true)
              | SOME (object, False) => SOME (object, false)
              | _ => NONE

        fun marshallReal (object: t, key: string) : (t * real) option =
            case marshall (object, key) of
                SOME (object, Real r) => SOME (object, r)
              | _ => NONE

        fun 'a marshallList (object: t, key: string, marshallFunc: t -> (t * 'a) option) : (t * 'a list) option =
            let
                fun doMarshallList (l: t list, accum: 'a list) : (t * 'a list) option =
                    case l of
                        [] => SOME (object, List.rev accum)
                      | hd :: tl =>
                    case marshallFunc (hd) of
                        SOME (object, res) => doMarshallList (tl, res :: accum)
                      | _ => NONE
            in
                case marshall (object, key) of
                    SOME (object, List l) => doMarshallList (l, [])
                  | _ => NONE
            end
    end

    end
end
