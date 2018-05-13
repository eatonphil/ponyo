structure Main =
struct
    local

    open Ponyo

    structure Os = Os
    structure Cli = Os.Cli
    structure File = Os.Filesystem.File

    in

    val mainFlag       = Cli.Flag.Anon "main"
    val filesFlag      = Cli.Flag.Named ("f", "files")
    val outputFlag     = Cli.Flag.Named ("o", "output")
    val backendFlag    = Cli.Flag.Named ("b", "backend")
    val includeFlag    = Cli.Flag.Named ("I", "include")
    val noPonyoFlag    = Cli.Flag.Named ("n", "no-ponyo")
    val saveTmpFlag    = Cli.Flag.Named ("s", "save-tmp")
    val workingDirFlag = Cli.Flag.Named ("C", "working-dir")

    val spec =
        let
            open Cli

            val mainDesc       = "main file to compile"
            val filesDesc      = "additional files to compile"
            val outputDesc     = "name of resulting binary"
            val includeDesc    = "list of Standard ML files to include in build"
            val noPonyoDesc    = "prevent default inclusion of Ponyo library in build"
            val saveTmpDesc    = "save temporary files created during build"
            val workingDirDesc = "change working directory to this directory"
            val backendDesc    = "compiler backend to use: [polyml, mlton]"
        in
            ("ponyo-make",
             "Ponyo-make builds Standard ML projects.",
             [(mainFlag, Arg.string, mainDesc)],
             [(filesFlag, Arg.list Arg.string, filesDesc),
              (outputFlag, Arg.optional (Arg.string, "./a.out"), outputDesc),
              (backendFlag, Arg.optional (Arg.string, "polyml"), backendDesc),
              (includeFlag, Arg.list Arg.string, includeDesc),
              (noPonyoFlag, Arg.optional (Arg.bool, "false"), noPonyoDesc),
              (saveTmpFlag, Arg.optional (Arg.bool, "false"), saveTmpDesc),
              (workingDirFlag, Arg.optional (Arg.string, ""), workingDirDesc)])
        end

    fun exec (program: string, args: string list) : Basis.OS.Process.status =
        Basis.OS.Process.system (program ^ " " ^ String.join(args, " "));
    
    fun writeTmpAndMake (buildFile: string, buildScript: string, doMake, saveTmp: bool) : unit =
        let
            val status = ref Basis.OS.Process.success
        in
            File.writeTo (buildFile, buildScript);
            status := doMake ();
            if saveTmp then
                ()
            else
                Basis.OS.FileSys.remove (buildFile);
            if Basis.OS.Process.isSuccess (!status) then
                ()
            else
                Basis.OS.Process.exit (!status)
        end
    
    fun makePolyML (main: string, sources: string list, ponyoRoot: string,
                    libraries: string list, binaryName: string,
                    saveTmp: bool) : unit =
        let
            val buildFile = "/tmp/ponyo_build.sml"
            val libraries =
                if ponyoRoot <> "" then
                    Os.Path.join [ponyoRoot, "build.sml"] :: libraries
                else
                    libraries
            val librariesList = String.join (libraries @ sources, "\",\"")
            val buildScript =
                Format.sprintf "map PolyML.make [\"%\"]; oldUse \"%\""
                               (librariesList :: [main])
            fun make () =
                exec ("polyc", ["-o", binaryName, buildFile])
        in
            writeTmpAndMake (buildFile, buildScript, make, saveTmp)
        end
    
    fun makeMLton (main: string, sources: string list, ponyoRoot: string,
                   libraries: string list, binaryName: string,
                   saveTmp: bool) : unit =
        let
            val buildFile = "/tmp/ponyo_build.mlb"
    
            (* MLton does not automatically call main. *)
            val mainShim = "/tmp/ponyo_build_main.sml"
            val _ = File.writeTo (mainShim, "val _ = main ()");
    
            val libraries =
                if ponyoRoot <> "" then
                    Os.Path.join [ponyoRoot, "build.mlb"] :: libraries
                else
                    libraries
            val librariesList = String.join (libraries, "\n")
            val sources =
                (map (fn (source) =>
                    if String.charAt (source, 0) = #"/"
                        then source
                    else
                        Os.Path.join [Basis.OS.FileSys.getDir (), source]) (sources @ [main]))
            val buildScript = String.join (librariesList :: sources @ [mainShim], "\n")
    
            fun make () =
                exec ("mlton", ["-output", binaryName, buildFile])
        in
            writeTmpAndMake (buildFile, buildScript, make, saveTmp)
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

            fun getAnon (flag) = Cli.getAnon (args, flag)
            fun getNamed (flag) = Cli.getNamed (args, flag)

            val [main] = getAnon (mainFlag)
            val sources = getNamed (filesFlag)

            val [noPonyo] = getNamed (noPonyoFlag)
            val [saveTmp] = getNamed (saveTmpFlag)
            val [workingDir] = getNamed (workingDirFlag)
            val [output] = getNamed (outputFlag)
            val [backend] = getNamed (backendFlag)
            val libraries = getNamed (includeFlag)

            val ponyoLib =
                if noPonyo = "true"
                    then ""
                else
                    getPonyoRoot ()

            val make =
                case backend of
                    "polyml" => makePolyML
                  | "mlton" => makeMLton
                  | _ => (Format.printf "ERROR: Bad backend.\n\n" []; Cli.doHelp (spec); makePolyML)
        in
            if workingDir <> "" then
                Basis.OS.FileSys.chDir (workingDir)
            else
                ();
            make (main, sources, ponyoLib, libraries, output, saveTmp = "true")
        end
    end
end

val main = Main.main
