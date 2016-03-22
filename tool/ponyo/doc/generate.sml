structure Format = Ponyo.Format

structure FileSystem = Ponyo.Os.FileSystem
structure File = Ponyo.Os.FileSystem.File
structure FilePath = Ponyo.Os.Path

structure String = Ponyo.String

structure Ast = Ponyo.Sml.Ast
structure Lexer = Ponyo.Sml.Lexer
structure Parser = Ponyo.Sml.Parser

structure Os = Ponyo.Os

structure StringMap = Ponyo_Container_Map(String)

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

fun generateHtml (outDir: string, asts: Ast.t StringMap) : unit =
    ()

fun generateDocumentation (directory: string, outDir: string) : unit =
    let
        val asts : Ast.t StringMap ref = ref StringMap.empty
        fun parseSignature (path: string) : unit =
            case FilePath.extension (path) of
                ".ML" => asts := AstMap.insert asts (path, parseFile (path))
              | _     => ()
    in
        FilePath.walk (directory, parseSignature);
        generateHtml (outDir, !asts)
    end
