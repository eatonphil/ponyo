structure Lexer = Ponyo.Sml.Lexer
structure Token = Ponyo.Sml.Token

fun lex (source: string) : Token.t list =
    let val stream = TextIO.openString (source) in
        Lexer.lex (fn () => TextIO.input1 (stream))
        handle Fail s => (Format.println [s]; [])
    end

fun print (t) =
    Format.println [Token.toString t]

fun main () = (
    Lexer.debug := true;
    map print (lex "14+2");
    map print (lex "(*foobar*)\n\"foo\" 1 + 1");
    ()
)
