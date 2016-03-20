structure Os = Ponyo.Os
structure Cli = Ponyo.Os.Cli
structure File = Ponyo.Os.FileSystem.File

fun exec (program: string, args: string list) : Basis.Os.Process.status =
    Basis.Os.Process.system (program ^ " " ^ String.join(args, " "));

fun make (program: string, libraries: string list, binaryName: string, saveTmp: bool) : unit =
    let
        val buildFile = "/tmp/ponyo_build.sml"
        val librariesList = String.join (libraries, "\",\"")
        val buildScript =
            Format.sprintf "map PolyML.make [\"%\"]; use \"%\""
            [librariesList, program]

        val status : Basis.Os.Process.status ref = ref Basis.Os.Process.success
        fun doMake () =
            status := exec ("polyc", ["-o", binaryName, buildFile])
            
    in
        File.writeTo (buildFile, buildScript);
        doMake ();
        if saveTmp then () else Basis.Os.FileSys.remove (buildFile);
        if Basis.Os.Process.isSuccess (!status) then ()
        else Basis.Os.Process.exit (!status)
    end

fun findMain (path: string) : string =
    path

structure Main =
struct
    val programFlag    = Cli.Flag.Anon "program"
    val outputFlag     = Cli.Flag.Named ("o", "output")
    val includeFlag    = Cli.Flag.Named ("I", "include")
    val noPonyoFlag    = Cli.Flag.Named ("n", "no-ponyo")
    val saveTmpFlag    = Cli.Flag.Named ("s", "save-tmp")
    val workingDirFlag = Cli.Flag.Named ("C", "working-dir")

    val spec =
        let
            open Cli

            val programDesc    = "name of (or directory containing) main"
            val outputDesc     = "name of resulting binary"
            val includeDesc    = "list of Standard ML files to include in build"
            val noPonyoDesc    = "prevent default inclusion of Ponyo library in build"
            val saveTmpDesc    = "save temporary files created during build"
            val workingDirDesc = "change working directory to this directory"
        in
            ("ponyo-make",
             "Ponyo-make builds Standard ML projects.",
             [(programFlag, Arg.string, programDesc)],
             [(outputFlag, Arg.optional (Arg.string, "./a.out"), outputDesc),
              (includeFlag, Arg.list Arg.string, includeDesc),
              (noPonyoFlag, Arg.optional (Arg.bool, "false"), noPonyoDesc),
              (saveTmpFlag, Arg.optional (Arg.bool, "false"), saveTmpDesc),
              (workingDirFlag, Arg.optional (Arg.string, ""), workingDirDesc)])
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

            fun getAnon (flag) = Cli.getAnon (args, flag)
            fun getNamed (flag) = Cli.getNamed (args, flag)

            val [program] = getAnon (programFlag)

            val program =
                if String.hasSuffix (program, ".sml") then program
                else findMain (program)

            val [noPonyo] = getNamed (noPonyoFlag)
            val [saveTmp] = getNamed (saveTmpFlag)
            val [workingDir] = getNamed (workingDirFlag)
            val [output] = getNamed (outputFlag)
            val libraries = getNamed (includeFlag)

            val ponyoLib =
                if noPonyo = "true" then ""
                else Os.Path.join ([getPonyoRoot (), "build.sml"])
        in
            if workingDir <> "" then Basis.Os.FileSys.chDir (workingDir) else ();
            make (program, ponyoLib :: libraries, output, saveTmp="true")
        end
end

val main = Main.main
