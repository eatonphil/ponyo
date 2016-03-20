structure Format = Ponyo.Format
structure File = Ponyo.Os.File
structure FilePath = Ponyo.Os.Path

structure Ast = Ponyo.Sml.Ast
structure Lexer = Ponyo.Sml.Lexer
structure Parser = Ponyo.Sml.Parser

structure Os = Ponyo.Os

fun parseFile (path: string) : Ast.t =
    let
        val file = String.join (File.readFrom path, "")
        val file = String.replace (file, "\n", "")
        val stream = TextIO.openString (file)
        fun lex () = Lexer.lex (fn () => TextIO.input1 stream)

        val tokens = lex () handle
            Fail reason => (Format.println [reason]; Ast.Root [])
    in
        Parser.parse (tokens) handle
            Fail reason => (Format.println [reason]; Ast.Root [])
    end

fun generateDocumentation (directory: string, outDir: string) : unit =
    let
        fun parseSignature (path: string) : unit =
            case FilePath.ext (path)
    in
        FilePath.walk (directory, parseSignature)
    end
