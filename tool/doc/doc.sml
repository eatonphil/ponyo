structure Main =
struct
    local
        structure Lexer = Ponyo.Sml.Lexer
        structure Parser = Ponyo.Sml.Parser

        structure Cli = Ponyo.Os.Cli
        structure File = Ponyo.Os.Filesystem.File

        structure Format = Ponyo.Format
        structure String = Ponyo.String
    in

    val directoryFlag    = Cli.Flag.Anon "directory"
    val outDirFlag       = Cli.Flag.Named ("o", "out-dir")
    val debugFlag        = Cli.Flag.Named ("d", "debug")
    val pageTemplateFlag = Cli.Flag.Named ("p", "page-template")
    val repositoryFlag   = Cli.Flag.Named ("r", "repository")

    val pageTemplateDefault =
        "<html>" ^
            "<head>" ^
                "<title>% | Ponyo Documentation</title>" ^
            "</head>" ^
            "<body>%</body>" ^
        "</html>"

    val spec =
        let
            open Cli

            val directoryDesc = "directory root for scanner"
            val outDirectoryDesc = "directory for generated HTML"
            val debugDesc = "parser debugging"
            val pageTemplateDesc = "file containing HTML template for generated pages"
            val repositoryDesc = "comma-separated list (e.g. github.com,eatonphil,ponyo)"
        in
            ("ponyo-doc",
             "Ponyo-doc converts signatures in .sig files to HTML files.",
             [(directoryFlag, Arg.string, directoryDesc)],
             [(outDirFlag, Arg.Optional (Arg.string, "./"), outDirectoryDesc),
              (debugFlag, Arg.Optional (Arg.bool, "false"), debugDesc),
              (pageTemplateFlag, Arg.Optional (Arg.string, ""), pageTemplateDesc),
              (repositoryFlag, Arg.Optional (Arg.string, ""), repositoryDesc)])
        end

    fun main () =
        let
            val args = Cli.getArgs (spec) handle
                Fail reason =>
                    (Format.printf "ERROR: %\n\n" [reason]; Cli.doHelp (spec); [])

            fun getAnon (flag) = Cli.getAnon (args, flag)
            fun getNamed (flag) = Cli.getNamed (args, flag)

            val [inDirectory] = getAnon (directoryFlag)
            val [outDirectory] = getNamed (outDirFlag)
            val [debug] = getNamed (debugFlag)
            val [pageTemplateFile] = getNamed (pageTemplateFlag)
            val [repository] = getNamed (repositoryFlag)
            val (host, namespace, project) = case String.split (repository, "/") of
                host :: (namespace :: (project :: [])) => (host, namespace, project)
              | _ => (Format.println ["ERROR: Bad repository.\n\n"];
                      Cli.doHelp (spec); ("", "", ""))

            val pageTemplate = String.join (File.readFrom (pageTemplateFile), "") handle
                _ => pageTemplateDefault
        in
            if debug = "true"
                then (Lexer.debug := true; Parser.debug := true)
            else ();

            Generate.inDirectory := inDirectory;
            Generate.outDirectory := outDirectory;
            Generate.pageTemplate := pageTemplate;
            Generate.repository := (host, namespace, project);
            
            Generate.generateDocumentation () handle
                Fail reason => Format.println [reason]
              | _ => Format.println ["Error."]
        end

    end
end

val main = Main.main
