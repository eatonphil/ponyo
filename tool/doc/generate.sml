structure Generate =
struct
    local
        structure Format = Ponyo.Format

        structure Filesystem = Ponyo.Os.Filesystem
        structure File = Ponyo.Os.Filesystem.File
        structure Path = Ponyo.Os.Path

        structure String = Ponyo.String

        structure Ast = Ponyo.Sml.Ast
        structure Lexer = Ponyo.Sml.Lexer
        structure Parser = Ponyo.Sml.Parser
        structure Token = Ponyo.Sml.Token

        type comment = string * string * string list
    in

    val inDirectory  = ref ""
    val outDirectory = ref ""
    val pageTemplate = ref ""
    val repository   = ref ("", "", "")

    fun sourceLink (path: string) : string =
        let
            val (host, namespace, project) = !repository

            val githubSource =
                Format.sprintf "https://github.com/%/%/blob/master"
                [namespace, project]
        in
            case host of
                "github.com" => Path.join [githubSource, path]
              | _ => raise Fail (Format.sprintf "Unknown provider %." [host]; "")
        end

    fun parseComments (tokens: Token.t list) : comment String.Dict.t =
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
                String.replace (String.stripWhitespace desc, "\n", " ")

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
                                map String.stripWhitespace (String.split (String.stripWhitespace examples, "\n")))
                      | _ => SOME (String.stripWhitespace name,
                                   cleanDescription comment, []))
                  | _ => NONE

            fun doParseComments (comments: string list, parsed: comment String.Dict.t) : comment String.Dict.t =
                case comments of
                    [] => parsed
                | comment :: comments =>
                case parseComment (comment) of
                    NONE => doParseComments (comments, parsed)
                | SOME (comment as (name, _, _)) =>
                    doParseComments (comments, String.Dict.insert parsed name comment)

        in
            doParseComments (comments, String.Dict.new ())
        end

    fun parseFile (path: string) : Ast.t * comment String.Dict.t =
        let
            val file = String.join (File.readFrom path, "")
            val file = String.replace (file, "\n", " ")
            val stream = TextIO.openString (file)
            fun lex () = Lexer.lex (fn () => TextIO.input1 stream)

            val tokens = lex () handle
                Fail reason => (Format.println [reason]; [])

            val ast = Parser.parse (tokens) handle
                Fail reason => (Format.printf "FAILURE [%]: %\n" [path, reason]; Ast.Root [])

            val commentsByName = parseComments (tokens)
        in
            (ast, commentsByName)
        end

    fun generatePage (ast: Ast.t, comments: comment String.Dict.t, source: string) : string =
        let
            fun generateSignature (signatureToken, body) =
                let
                    val name = Token.toString (signatureToken)

                    val (_, description, examples) = case String.Dict.get comments name of
                        NONE => ("", "", [])
                      | SOME comment => comment

                    val sigHtml =
                        "<div class='ponyo-signature' id='%'>" ^
                            (if source = "" then "<span>%</span><h2>%</h2>" else
                             "<h2>%</h2>") ^
                            "<a href='%' target='_blank'>Source</a>" ^
                            "<div class='ponyo-signature-information'>" ^
                                (if description = "" then "<span>%</span>" else
                                "<div class='ponyo-signature-description'>%</div>") ^
                                (if length examples = 0 then "<span>%</span>" else
                                "<div class='ponyo-signature-examples'>" ^
                                    "<div class='ponyo-signature-examples-label'>Examples:</div>" ^
                                    "<pre><code class='sml ocaml'>%</code></pre>" ^
                                "</div>") ^
                            "</div>" ^
                            "<div class='ponyo-signature-body'>%</div>" ^
                        "</div>"

                    val sigValues = [
                        name,
                        name,
                        source,
                        description,
                        String.join (examples, "\n"),
                        generatePage (body, comments, source)
                    ]
                in
                    Format.sprintf sigHtml sigValues
                end

            fun generateGeneric (genericType, valueToken, ty) =
                let
                    val name = Token.toString (valueToken)

                    val (_, description, examples) = case String.Dict.get comments name of
                        NONE => ("", "", [])
                      | SOME comment => comment

                    val valueHtml =
                        "<div class='ponyo-generic'>" ^
                            "<div class='ponyo-generic-signature'>" ^
                                "<span class='ponyo-generic-name-label-type'>%</span>" ^
                                "<span class='ponyo-generic-name'>%</span>" ^
                                (if ty = Ast.NoType then "<span>%</span>" else
                                "<span class='ponyo-generic-name-label-colon'>:</span>" ^
                                "<span class='ponyo-generic-type'>%</span>") ^
                            "</div>" ^
                            (if description = "" then "<span>%</span>" else
                            "<div class='ponyo-generic-description'>%</div>") ^
                            (if length examples = 0 then "<span>%</span>" else
                            "<div class='ponyo-generic-examples'>" ^
                                "<div class='ponyo-generic-examples-label'>Examples:</div>" ^
                                "<pre><code class='sml ocaml'>%</code></pre>" ^
                            "</div>") ^
                        "</div>"

                    val valueValues = [
                        genericType,
                        name,
                        if ty = Ast.NoType then "" else Ast.tyToString ty,
                        description,
                        String.join (examples, "\n")
                    ]
                in
                    Format.sprintf valueHtml valueValues
                end
        in
            case ast of
                Ast.Root (children) =>
                String.join (map (fn child => generatePage (child, comments, source)) children, "")
              | Ast.Signature (name, body) =>
                  generateSignature (name, body)
              | Ast.SignatureBody (children) =>
                  String.join (map (fn child => generatePage (child, comments, source)) children, "")
              | Ast.ValueDec (name, (Ast.Type ty)) => generateGeneric ("val", name, ty)
              | Ast.TypeDec (name, (Ast.Type ty)) => generateGeneric ("type", name, ty)
              | Ast.EqtypeDec (name) => generateGeneric ("eqtype", name, Ast.NoType)
              | _ => ""
        end

    fun writePage (path: string, body: string) : unit =
        let
            val page = Format.sprintf (!pageTemplate)
                       [Path.base (Path.file path),
                       body]
                       
            val pageDir = Path.directory (path)
        in
            if Filesystem.exists (pageDir)
                then ()
            else Filesystem.makeDirectory (pageDir);
            File.writeTo (path, page)
        end

    fun generateHtml (asts: (Ast.t * comment String.Dict.t) String.Dict.t) : unit =
        let
            fun outFile (path) =
                Path.join [!outDirectory, Basis.OS.Path.base path ^ ".html"]

            fun generatePages (astList: (string * (Ast.t * comment String.Dict.t)) list) : unit =
                case astList of
                    [] => ()
                  | (path, (ast, comments)) :: astList =>
                let in
                    writePage (outFile path, generatePage (ast, comments, sourceLink path));
                    generatePages (astList)
                end
        in
            generatePages (String.Dict.toList asts)
        end

    fun parseSignature (path: string) (asts: (Ast.t * comment String.Dict.t) String.Dict.t) : (Ast.t * comment String.Dict.t) String.Dict.t =
        if Path.extension (path) <> "sig"
            then asts
        else if Path.file (path) = "ml_bind.ML"
            then asts
        else String.Dict.insert
             asts
             (String.substringToEnd (path, String.length (!inDirectory)))
             (parseFile path)

    fun generateDocumentation () : unit =
        let
            val asts = Filesystem.walkWith (!inDirectory) parseSignature (String.Dict.new ())
        in
            generateHtml asts
        end

    end
end
