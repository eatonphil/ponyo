structure Format = Ponyo.Format
structure String = Ponyo.String

structure Cli = Ponyo.Os.Cli
structure Filesystem = Ponyo.Os.Filesystem
structure Path = Ponyo.Os.Path

fun exec (program: string, args: string list) : unit =
    Basis.Posix.Process.exec (program, args)

structure Main =
struct
    val polyFlag = Cli.Flag.Named ("p", "poly")
    val evalFlag = Cli.Flag.Named ("e", "eval")

    val spec =
        let
            open Cli

            val polyDesc = "location of poly binary if not in PATH"
            val evalDesc = "evalautes and exits"
        in
            ("ponyo-top",
             "Ponyo-top gives access to the Poly top-level with pre-built Ponyo libraries.",
             [],
             [
              (polyFlag, Arg.optional (Arg.string, ""), polyDesc),
              (evalFlag, Arg.optional (Arg.string, ""), evalDesc)
             ])
        end

    fun getPonyoRoot () =
        case Basis.OS.Process.getEnv "PONYO_ROOT" of
            SOME root => root
          | NONE => "./"

    fun main () =
        let
            val args = Cli.getArgs (spec) handle
                Fail reason =>
                     (Format.printf "ERROR: %\n\n" [reason]; Cli.doHelp (spec); [])
            val [polyPath] = Cli.getNamed (args, polyFlag)
            val [eval] = Cli.getNamed (args, evalFlag)

            val ponyoLib = Path.join ([getPonyoRoot (), "build.sml"])
            val polyExecutable =
                if Filesystem.exists (polyPath) then polyPath
                else case Filesystem.which "poly" of
                    NONE => (Format.println ["poly must be present in PATH"]; "")
                  | SOME path => path

            val makeLine = Format.sprintf "PolyML.make \"%\"; val _ = print \"Ponyo-top top-level ready!\\n\";" [ponyoLib]
            fun evalLine () = Format.sprintf "PolyML.make \"%\"; %" [ponyoLib, eval]

            (* The first argument is the arg0 which is the name of the program.
             * This is here intentionally. *)
            val toExec = ["poly", "--eval", if eval = "" then makeLine else evalLine ()]
        in
            exec (polyExecutable, toExec)
        end
end

val main = Main.main
