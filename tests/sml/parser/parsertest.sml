structure File = Ponyo.Os.File
structure Format = Ponyo.Format

structure Ast = Ponyo.Sml.Ast
structure Lexer = Ponyo.Sml.Lexer
structure Parser = Ponyo.Sml.Parser

fun parse (path: string) : Ast.t =
    let
        val file = String.join (File.readFrom path, "")
        val file = String.replace (file, "\n", " ")
        val stream = TextIO.openString (file)
        fun lex () = Lexer.lex (fn () => TextIO.input1 stream)
    in
        Parser.parse (lex () handle Fail s => (Format.println [s]; []))
        handle Fail s => (Format.println [s]; Ast.Root [])
    end

fun main () = (
    Ast.print (parse "tests/sml/parser/test.sml");
    ()
)
