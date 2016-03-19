structure Cli = Ponyo.Os.Cli

fun ponyo (program: string, args: string list) : unit =
    let
        val exec = Posix.Process.exec
    in
        exec (program, args);
        ()
    end

structure Main =
struct
    val commandFlag = Cli.Flag.Anon "command"

    val spec =
        let
            open Cli

            val commandDesc = "options: make | doc | help | version"
        in
            ("ponyo",
             "Ponyo is a tool for working with Standard ML programs.",
             [(commandFlag, Arg.string, commandDesc)],
             [])
        end

    fun main () =
        let
            val args = Cli.getArgs (spec)
            val [cmd] = Cli.getAnon (args, commandFlag)
            val rest = Cli.getRest (args)
        in
            case cmd of
                "doc" => ponyo (cmd, rest)
              | "make" => ponyo (cmd, rest)
              | "help" => Cli.doHelp (spec)
              | "version" => Format.println ["ponyo version ponyo-0.1 freebsd/amd64"]
              | _ => ()  
        end
end

val main = Main.main
