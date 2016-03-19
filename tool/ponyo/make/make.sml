structure Cli = Ponyo.Os.Cli
structure File = Ponyo.Os.File

fun exec (program: string, args: string list) : unit =
    let in
        Format.printf "% %\n" [program, String.join (args, " ")];
        Basis.Os.Process.system (program ^ " " ^ String.join(args, " "));
        ()
    end

fun make (program: string, libraries: string list, binaryName: string) : unit =
    let
        val buildFile = "/tmp/ponyo_build.sml"

        val ponyoRoot = "./"
        val libraries =
            String.join ([ponyoRoot ^ "build.sml"] @ libraries, "\",\"")
        val buildScript =
            Format.sprintf "map PolyML.make [\"%\"]; use \"%\""
            [libraries, program]
    in
        File.writeTo (buildFile, buildScript);
        exec ("polyc", ["-o", binaryName, buildFile]);
        Basis.Os.FileSys.remove (buildFile);
        ()
    end

fun findMain (path: string) : string =
    path

structure Main =
struct
    val programFlag = Cli.Flag.Anon "program"
    val outputFlag = Cli.Flag.Named ("o", "output")
    val includeFlag = Cli.Flag.Named ("I", "include")

    val spec =
        let
            open Cli

            val programDesc = "name of (or directory containing) main"
            val outputDesc = "name of resulting binary"
            val includeDesc = "list of Standard ML files to also build"
        in
            ("ponyo-make",
             "Ponyo-make builds Standard ML projects.",
             [(programFlag, Arg.string, programDesc)],
             [(outputFlag, Arg.optional (Arg.string, "./a.out"), outputDesc),
              (includeFlag, Arg.list Arg.string, includeDesc)])
        end

    fun main () =
        let
            val args = Cli.getArgs (spec) handle
                Fail s => (Format.printf "ERROR: %\n\n" [s]; Cli.doHelp (spec); [])

            fun getAnon (flag) = Cli.getAnon (args, flag)
            fun getNamed (flag) = Cli.getNamed (args, flag)

            val [program] = getAnon (programFlag)

            val program =
                if String.hasSuffix (program, ".sml") then program
                else findMain (program)

            val [output] = getNamed (outputFlag)
            val libraries = getNamed (includeFlag)
        in
            make (program, libraries, output)
        end
end

val main = Main.main
