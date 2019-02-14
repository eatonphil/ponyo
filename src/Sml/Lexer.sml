structure Ponyo_Sml_Lexer =
struct
    local
        structure String = Ponyo_String
        structure Format = Ponyo_Format_String
        structure Token = Ponyo_Sml_Token
        structure StringList = Ponyo_Container_List (String)
    in

    val debug = ref false

    type stream = unit -> char option

    type reader = {
        stream: stream,
        store: string,
        line: int,
        col: int
    }

    type tokens = Token.t list

    type readerReadOpt = reader * string option
    type readerTokenOpt = reader * Token.t option

    infix >>=;
    fun (reader, s) >>= f : readerTokenOpt =
        case s of
            NONE        => (reader, NONE)
          | SOME s => f (reader, s)

    fun sListLongest (sList: string list) : string =
        foldl (fn (s, longest) => if (String.length longest) > (String.length s)
                                  then longest else s) "" sList

    fun stringBeforeStream (chars: string, stream: stream) : stream =
        let
            val streamed = ref chars
            fun next () =
                if !streamed = "" then NONE
                else let val c = SOME (String.toChar (!streamed)) in
                    streamed := String.substringToEnd (!streamed) 1;
                    c
                end
        in
            fn () => case next () of
                NONE   => stream ()
              | SOME c => SOME c
        end

    (* Char.toString converts " to \", which is wrong in these cases. *)
    fun charToString (c: char) : string =
        case c of
            #"\"" => "\""
          | c     => Char.toString (c)

    val reservedWords = [
        "eqtype", "functor", "include", "sharing", "sig",
        "signature", "struct", "structure", "where", ":>",
        "abstype", "and", "andalso", "as", "case",
        "datatype", "do", "else", "end", "exception",
        "fn", "fun", "handle", "if", "in", "infix",
        "infixr", "let", "local", "nonfix", "of",
        "op", "open", "orelse", "raise", "rec",
        "then", "type", "val", "with", "withtype", "while",
        "(", ")", "[", "]", "{",
        "}", ",", ":", ";", "...",
        "_", "|", "=", "=>", "->", "#"
    ]

    fun readChars (reader: reader, guard: char -> bool) : readerReadOpt =
        let
            (* Adds stored characters to the stream as if they weren't read. *)
            val reader = {
                stream = stringBeforeStream (#store reader, #stream reader),
                store  = "",
                line = #line reader,
                col = #col reader
            }

            fun doRead (state as {stream, store, col, line}: reader, chars: string) : readerReadOpt =
                case stream () of
                    NONE => (state, if chars = "" then NONE else SOME (String.reverse chars))
                  | SOME c =>
                let
                    val isNewline = c = #"\n"
                in
                    if guard (c) then
                        doRead (state, charToString (c) ^ chars)
                    else if chars = "" then (
                        {stream=stream, store=charToString(c) ^ store, line=line, col=col},
                        NONE
                    )
                    else (
                        ({stream=stream, store=charToString (c), col=col, line=line},
                        SOME (String.reverse (chars ^ store)))
                    )
                end
        in
            doRead (reader, "")
        end

    fun readNumber (reader: reader) : readerTokenOpt =
        let
            fun isNumber (number: string) : bool =
                Option.isSome (Real.fromString number) orelse
                Option.isSome (Int.fromString number)
        in
            readChars (reader, Char.contains "~.Ee0123456789abcdef") >>=
            (fn (reader as {stream, store, line, col}, number) =>
                if isNumber (number) then
                    (reader, SOME ({ token = Token.Number number, line = line, col = col }))
                else
                    ({stream=stream, store=number ^ store, line=col, col=col}, NONE))
        end

    fun readString (reader: reader) : readerTokenOpt =
        let
            val seen = ref ""
            val illegal = (Format.sprintf "Illegal newline in string: {}." [!seen])

            fun lastChar () = String.charAt (!seen) (String.length (!seen) - 2)
            fun firstChar () = String.charAt (!seen) 0

            fun isString (c: char) : bool = (
                seen := !seen ^ (if c = #"\"" then "\"" else charToString c);

                if firstChar () = #"\"" then case c of
                    #"\n" => raise Fail illegal
                  | #"\"" =>
                      if !seen = "\"" then true
                      else if lastChar () = #"\\" then true
                      else false (* Final quote must be eaten manually below. *)
                  | c => true
                else false
            )

            fun stringMinusMarkers () =
                String.substring (!seen) 1 (String.length (!seen) - 2)
        in
            readChars (reader, isString) >>=
            (fn (reader as {stream, store, line, col}, _) =>
                (* Eat final quote stored in store. *)
                ({stream=stream, store="", line=col, col=col},
                 SOME (Token.String (stringMinusMarkers ()))))
        end

    fun readSymbol (reader: reader) : readerTokenOpt =
        let
            val seen = ref ""

            fun lastChar () : string =
                String.substringToEnd (!seen) (String.length (!seen) - 1)

            fun butLastChar () : string =
                String.substring (!seen) 0 (String.length (!seen) - 1)

            fun isPrefix () : bool =
                foldl (fn (word, isPrefix) =>
                    isPrefix orelse String.hasPrefix word (!seen)) false reservedWords

            fun charTypeChanged () : bool =
                if String.isAlphaNum (butLastChar ()) andalso
                    Char.isSpace (valOf (Char.fromString (lastChar ()))) then true
                else if String.isAlphaNum (butLastChar ()) then false
                else true

            fun isSymbol (c: char) : bool = (
                seen := !seen ^ (charToString c);
                if Char.isSpace (c) then false
                else if not (isPrefix ()) andalso charTypeChanged () then false
                else if String.length (sListLongest reservedWords) = String.length (!seen) then false
                else true
            )

        in
            readChars (reader, isSymbol) >>=
            (fn (reader1 as {stream, store, line, col}, ident) =>
                if StringList.contains (reservedWords, !seen)
                    then ({stream=stream, store="", line=line, col=col}, SOME (Token.Symbol (!seen)))
                else if StringList.contains (reservedWords, ident)
                    then ({stream=stream, store=store, line=line, col=col}, SOME (Token.Symbol ident))
                else ({stream=stream, store=ident ^ store, line=line, col=col}, NONE))
        end

    fun readIdent (reader: reader) : readerTokenOpt =
        let
            val seen = ref ""

            fun isAlphaNumeric (c: char) : bool =
                Char.isAlphaNum (c) orelse Char.contains "'_." c
            fun isSymbolic (c: char) : bool = Char.contains "!%&$#+-/:<=>?@\\~'^|*" c
            fun firstChar () = if !seen = "" then #" " else String.charAt (!seen) 0

            fun isIdent (c: char) : bool = (
                seen := !seen ^ (charToString c);
                (* Don't break on first char if there isn't a first char. *)
                if List.all isAlphaNumeric (String.explode (!seen))
                    then Char.notContains "0123456789_" (firstChar ())
                else
                    List.all isSymbolic (String.explode (!seen))
            )
        in
            readChars (reader, isIdent) >>= (fn (reader, ident) =>
            (reader, SOME (Token.Ident ident)))
        end

    fun readComment (reader: reader) : readerTokenOpt =
        let
            val seen = ref ""
            fun seenLen () = String.length (!seen)

            fun firstChar () = String.toChar (!seen)
            fun secondChar () = String.charAt (!seen) 1
            fun lastChar () = String.charAt (!seen) (String.length (!seen) - 2)

            fun isComment (c: char) : bool = (
                seen := !seen ^ charToString (c);
                if firstChar () <> #"(" then false
                else if seenLen () > 1 andalso secondChar () <> #"*" then false
                (* Last ending paren must be manually eaten below. *)
                else if c = #")" andalso lastChar () = #"*" then false
                else true
            )

            fun commentsMinusMarkers () =
                String.substring (!seen) 2 (seenLen () - 4)
        in
            readChars (reader, isComment) >>=
            (fn (reader as {stream, store, line, col}, comment) =>
                (* Eat final paren stored in store. *)
                if secondChar () = #"*"
                    then ({stream=stream, store="", line=line, col=col},
                          SOME (Token.Comment (commentsMinusMarkers ())))
                else
                     ({stream=stream, store=comment ^ store, line=line, col=col}, NONE))
        end

    fun readWhitespace (reader: reader) : reader =
        let
            val seen = ref ""

            fun lastChar () =
                String.charAt (!seen) (String.length (!seen) - 1)

            fun isEscape () : bool =
                let
                    val firstChar = String.toChar (!seen)
                in
                    String.length (!seen) = 1 andalso firstChar = #"\\" orelse
                    String.length (!seen) = 2 andalso
                        Char.contains "nbtr" (lastChar ()) andalso
                        firstChar = #"\\"
                end

            fun isSpace (c: char) : bool = (
                seen := !seen ^ (charToString c);
                List.all Char.isSpace (String.explode (!seen)) orelse isEscape ()
            )
        in
            case readChars (reader, isSpace) of
                (reader, _) => reader
        end

    fun lex (stream: stream) : tokens =
        let
            val lexers = [
                ("comment", readComment),
                ("number", readNumber),
                ("string", readString),
                ("symbol", readSymbol),
                ("ident", readIdent)
            ]

            fun printScan (readType, token) =
                Format.printf "Scanning %, got: \"%\"\n" [readType, Token.toString token]

            fun doLex ((readType, lex), (reader, tokens)) : reader * tokens =
                case lex (readWhitespace reader) of
                    (reader, SOME token) => (
                        if !debug then printScan (readType, token) else ();
                        doLexAll (reader, token :: tokens)
                    )
                  | (reader, NONE) => (reader, tokens)

            and doLexAll rt : reader * tokens =
                foldl doLex rt lexers

            val (reader, tokens) =
                doLexAll ({stream=stream, store="", line=0, col=0}, [])
        in
            case (#stream reader) () of
                SOME c => raise Fail "Invalid lexical construct 1"
              | NONE   =>
            case (#store reader) of
                "" => List.rev (tokens)
              | s  => raise Fail "Invalid lexical construct 2"
        end

    end
end
