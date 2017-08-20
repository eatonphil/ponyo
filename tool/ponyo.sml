structure Main =
struct
    local
        structure Cli = Ponyo.Os.Cli

        structure String = Ponyo.String
        structure Format = Ponyo.Format
    in

    fun ponyo (program: string, args: string list) : unit =
        let in
            Basis.OS.Process.system ("ponyo-" ^ program ^ " " ^ String.join(args, " "));
            ()
        end

    val commandFlag = Cli.Flag.Anon "command"

    val spec =
        let
            open Cli

            val commandDesc = "options: make | top | doc | help | version"
        in
            ("ponyo",
             "Ponyo is a tool for working with Standard ML programs.",
             [(commandFlag, Arg.string, commandDesc)],
             [])
        end

    fun main () =
        let
            val args = Cli.getArgs (spec) handle
                Fail reason =>
                    (Format.printf "ERROR: %\n\n" [reason]; Cli.doHelp (spec); [])
            val [cmd] = Cli.getAnon (args, commandFlag)
            val rest = tl (CommandLine.arguments ())
        in
            case cmd of
                "doc"     => ponyo (cmd, rest)
              | "make"    => ponyo (cmd, rest)
              | "top"     => ponyo (cmd, rest)
              | "test"    => ponyo (cmd, rest)
              | "help"    => Cli.doHelp (spec)
              | "version" => Format.println ["ponyo version ponyo-0.1"]
              | _ => (Format.printf "ERROR: Bad command [%].\n\n" [cmd]; Cli.doHelp (spec))
        end

    end
end

val main = Main.main
