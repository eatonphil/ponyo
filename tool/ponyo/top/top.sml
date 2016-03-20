structure Format = Ponyo.Format
structure Os = Ponyo.Os
structure Cli = Ponyo.Os.Cli

fun exec (program: string, args: string list) : unit =
    (Format.println [program ^ " " ^ String.join(args, " ")];
    Basis.Posix.Process.exec (program, args))

structure Main =
struct
    val polyFlag = Cli.Flag.Named ("p", "poly")

    val spec =
        let
            open Cli

            val polyDesc = "location of poly binary if not in PATH"
        in
            ("ponyo-top",
             "Ponyo-top gives access to the Poly top-level with pre-built Ponyo libraries.",
             [],
             [(polyFlag, Arg.optional (Arg.string, ""), polyDesc)])
        end

    fun getPonyoRoot () =
        case Basis.Os.Process.getEnv "PONYO_ROOT" of
            SOME root => root
          | NONE => "./"

    fun main () =
        let
            val args = Cli.getArgs (spec) handle
                Fail reason =>
                     (Format.printf "ERROR: %\n\n" [reason]; Cli.doHelp (spec); [])
            val [polyPath] = Cli.getNamed (args, polyFlag)

            val ponyoLib = Os.Path.join ([getPonyoRoot (), "build.sml"])
            val polyExecutable =
                if Os.Path.FilePath.exists (polyPath) then polyPath
                else case Os.Path.FilePath.which "poly" of
                    NONE => (Format.println ["poly must be present in PATH"]; "")
                  | SOME path => path

            val makeLine = Format.sprintf "PolyML.make \"%\"; val _ = print \"Ponyo-top top-level ready!\\n\";" [ponyoLib]
        in
            exec (polyExecutable, ["poly", "--eval", makeLine])
        end
end

val main = Main.main
