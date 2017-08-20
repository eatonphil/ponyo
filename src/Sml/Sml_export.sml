structure Ponyo_Sml =
struct
    local
        structure File = Ponyo_Os_Filesystem_File
        structure String = Ponyo_String
        structure Format = Ponyo_Format
    in

    structure Ast = Ponyo_Sml_Ast
    structure Lexer = Ponyo_Sml_Lexer
    structure Parser = Ponyo_Sml_Parser
    structure Token = Ponyo_Sml_Token

    fun parseString (source: string) : Ast.t =
        let
            val stream = TextIO.openString (source)
            fun lex () = Lexer.lex (fn () => TextIO.input1 stream)
            val tokens = lex ()
            val ast = Parser.parse (tokens)
        in
            ast
        end

    fun parseFile (path: string) : Ast.t =
        let
            val file = String.join (File.readFrom path, "")
        in
            parseString (String.replace (file, "\n", " "))
        end

    end
end
