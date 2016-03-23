structure Format = Ponyo.Format

structure FileSystem = Ponyo.Os.FileSystem
structure File = Ponyo.Os.FileSystem.File
structure Path = Ponyo.Os.Path

structure String = Ponyo.String

structure Ast = Ponyo.Sml.Ast
structure Lexer = Ponyo.Sml.Lexer
structure Parser = Ponyo.Sml.Parser

structure StringMap = Ponyo_Container_Map(String)

type comment = string * string * string list

fun parseComments (tokens: Token.t list) : comment StringMap.t =
    let
        val comments =
            foldl (fn (token, comments) => case token of
                Token.Comment comment => comment :: comments
              | _ => comments) [] tokens

        fun cleanComment (comment: string) : string =
            let
                val lines = String.split (comment, "*")
                val lines = map String.stripWhitespace lines
            in
                String.join (lines, "\n")
            end

        val comments = map cleanComment comments

        fun cleanDescription (desc: string) : string =
            String.replace (String.stripWhitespace (desc), "\n", " ")

        fun parseComment (comment: string) : comment option =
            case String.splitN (comment, ":", 1) of
                [] => NONE
              | name :: [comment] => (
            case String.splitN (comment, "Ex:", 1) of
                [] => SOME (String.stripWhitespace name,
                            cleanDescription comment, [])
              | description :: [examples] =>
                  SOME (String.stripWhitespace name,
                        cleanDescription description,
                        map String.stripWhitespace (String.split (examples, "\n")))
              | _ => SOME (String.stripWhitespace name,
                           cleanDescription comment, []))
              | _ => NONE

        fun doParseComments (comments: string list, parsed: comment StringMap.t) : comment StringMap.t =
            case comments of
                [] => parsed
              | comment :: comments =>
            case parseComment (comment) of
                NONE => doParseComments (comments, parsed)
              | SOME (comment as (name, _, _)) =>
                  doParseComments (comments, StringMap.insert parsed (name, comment))
              
    in
        doParseComments (comments, StringMap.empty)
    end

fun parseFile (path: string) : Ast.t * comment StringMap.t =
    let
        val file = String.join (File.readFrom path, "")
        val file = String.replace (file, "\n", " ")
        val stream = TextIO.openString (file)
        fun lex () = Lexer.lex (fn () => TextIO.input1 stream)

        val tokens = lex () handle
            Fail reason => (Format.println [reason]; [])

        val ast = Parser.parse (tokens) handle
            Fail reason => (Format.println [reason]; Ast.Root [])

        val commentsByName = parseComments (tokens)
    in
        (ast, commentsByName)
    end

fun generatePage (sigAst: Ast.t, comments: comment StringMap.t) : string =
    let
        fun generateValue (valueToken, ty) =
            let
                val name = Token.toString (valueToken)
                
                val (_, description, examples) = case StringMap.get comments name of
                    NONE => ("", "", [])
                  | SOME comment => comment

                val valueHtml = ("<div class='ponyo-value'>" ^
                                     "<div class='ponyo-value-signature'>" ^
                                         "<span class='ponyo-value-name-label-val'>val</span>" ^
                                         "<span class='ponyo-value-name'>%</span>" ^
                                         "<span class='ponyo-value-name-label-colon'>:</span>" ^
                                         "<span class='ponyo-value-type'>%</span>" ^
                                     "</div>" ^
                                     "<div class='ponyo-value-description'>%</div>" ^
                                 (if length examples = 0 then "%" else
                                     "<div class='ponyo-value-examples'>" ^
                                         "<div class='ponyo-value-examples-label'>Examples:</div>" ^
                                         "<pre><code class='sml ocaml'>%</pre></code>" ^
                                     "</div>") ^
                                 "</div>")
                val valueValues = [
                    name,
                    Ast.tyToString ty,
                    description,
                    String.join (examples, "\n")
                ]
            in
                Format.sprintf valueHtml valueValues
            end
    in
        case sigAst of
            Ast.Root (children) =>
              String.join (map (fn child => generatePage (child, comments)) children, "")
          | Ast.Signature (name, body) =>
              Format.sprintf ("<div class='ponyo-signature'>" ^
                                  "<h2>% Signature</h2>" ^
                                  "<div class='ponyo-signature-body'>%</div>" ^
                              "</div>") [Token.toString name, generatePage (body, comments)]
          | Ast.SignatureBody (children) =>
              String.join (map (fn child => generatePage (child, comments)) children, "")
          | Ast.ValueDec (name, (Ast.Type ty)) => generateValue (name, ty)
          | _ => ""
    end

fun writePage (path: string, page: string, body: string) : unit =
    File.writeTo (path, Format.sprintf page [Path.base (Path.file path), body])

fun generateHtml (outDir: string, page: string, asts: (Ast.t * comment StringMap.t) StringMap.t) : unit =
    let
        fun outFile (path) =
            Path.join [outDir, Basis.Os.Path.base path ^ ".html"]

        fun generatePages (astList: (string * (Ast.t * comment StringMap.t)) list) : unit =
            case astList of
                [] => ()
              | (path, ast) :: astList =>
            let in
                writePage (outFile path, page, generatePage ast);
                generatePages (astList)
            end
    in
        generatePages (StringMap.toList asts)
    end

fun generateDocumentation (inDir: string, page: string, outDir: string) : unit =
    let
        val asts : (Ast.t * (comment StringMap.t)) StringMap.t ref = ref StringMap.empty
        fun parseSignature (path: string) : unit =
            case Path.extension (path) of
                "ML" => asts := StringMap.insert
                    (!asts)
                    (String.substringToEnd (path, String.length inDir),
                     parseFile (path))
              | _ => ()
    in
        FileSystem.walk (inDir, parseSignature);
        generateHtml (outDir, page, !asts)
    end
