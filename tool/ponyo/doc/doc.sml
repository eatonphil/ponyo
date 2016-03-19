structure Lexer = Ponyo.Sml.Lexer
structure Parser = Ponyo.Sml.Parser

structure Cli = Ponyo.Os.Cli
structure Format = Ponyo.Format

structure Main =
struct
    val directoryFlag = Cli.Flag.Anon "directory"
    val outDirFlag = Cli.Flag.Named ("o", "out-dir")
    val debugFlag = Cli.Flag.Named ("d", "debug")

    val spec =
        let
            open Cli

            val directoryDesc = "directory root for scanner"
            val outDirectoryDesc = "directory for generated HTML"

            val debugDesc = "parser debugging"
        in
            ("ponyo-doc",
             "Ponyo-doc converts signatures in .ML files to HTML files.",
             [(directoryFlag, Arg.string, directoryDesc)],
             [(outDirFlag, Arg.Optional (Arg.string, "./"), outDirectoryDesc),
              (debugFlag, Arg.Optional (Arg.bool, "false"), debugDesc)])
        end

    fun main () =
        let
            val args = Cli.getArgs (spec) handle
                Fail reason =>
                    (Format.printf "ERROR: %\n\n" [reason]; Cli.doHelp (spec); [])

            fun getAnon (flag) = Cli.getAnon (args, flag)
            fun getNamed (flag) = Cli.getNamed (args, flag)

            val [directory] = getAnon (directoryFlag)
            val [outDir] = getNamed (outDirFlag)
            val [debug] = getNamed (debugFlag)
        in
            if debug = "true"
                then (Lexer.debug := true; Parser.debug := true)
            else ();
            
            generateDocumentation (directory, outDir)
        end
end

val main = Main.main
