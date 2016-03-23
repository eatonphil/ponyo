structure File = Ponyo.Os.FileSystem.File
structure Format = Ponyo.Format

structure Ast = Ponyo.Sml.Ast
structure Lexer = Ponyo.Sml.Lexer
structure Parser = Ponyo.Sml.Parser

fun parse (path: string) : Ast.t =
    let
        val file = String.join (File.readFrom path, "")
        val file = String.replace (file, "\n", " ")
        val stream = TextIO.openString (file)
        val tokens = Lexer.lex (fn () => TextIO.input1 stream) handle
            Fail s => (Format.println [s]; [])
    in
        Parser.parse (tokens) handle
            Fail s => (Format.println [s]; Ast.Root [])
    end

fun main () = (
    Ast.print (parse "test/Sml/parser/test.sml");
    ()
)
