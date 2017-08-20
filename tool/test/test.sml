structure Main =
struct
    local
        open Ponyo

        structure Cli        = Os.Cli
        structure Filesystem = Os.Filesystem
        structure File       = Filesystem.File
        structure Path       = Os.Path
    in

    val backendFlag = Cli.Flag.Named ("b", "backend")

    val spec =
        let
            open Cli

            val backendDesc = "compiler backend to use: [polyml, mlton]"
        in
            ("test",
             "Ponyo test runner.",
             [],
             [(backendFlag, Arg.optional (Arg.string, "polyml"), backendDesc)])
        end

    (* TODO: crash on failure *)
    fun exec (program: string) (args: string list) : Basis.OS.Process.status =
        Basis.OS.Process.system (program ^ " " ^ String.join(args, " "));

    fun findFiles (currentPath: string) (foundFiles: string list) : string list =
        if String.hasSuffix (currentPath, "_Test.sml")
            then currentPath :: foundFiles
        else foundFiles

    fun testFiles (directory) : string list =
        Filesystem.walkWith directory findFiles []

    fun generateTests (files: string list) : string =
        let
            fun generateRun (file: string) : string =
                Format.sprintf "test \"%\" (%.run ())" [Path.filename file, Path.filename file]
        in
            String.join (map generateRun files, ",\n")
        end

    fun generateFile (files: string list, backend: string) : string =
        Format.sprintf
        ("local open Ponyo; val test = Test.test in\n" ^
        "fun main () =\n" ^
        "    let\n" ^
        "        val _ = Format.println [\"Beginning tests with %.\"]\n" ^
        "        val tests = test \"All\" [%] handle e => (Format.println [exnName e, exnMessage e]; false)\n" ^
        "    in\n" ^
        "        if tests then Format.println [\"All tests passed!\"]\n" ^
        "        else (Format.println [\"Tests failed.\"]; Basis.OS.Process.exit (Basis.OS.Process.failure); ())\n" ^
        "    end\n" ^
        "end") (backend :: [generateTests files])

    fun writeFile (file: string) (contents: string) : unit =
        let
            val fileDir = Path.directory (file)
        in
            if Filesystem.exists (fileDir)
                then ()
            else
                Filesystem.makeDirectory (fileDir);
            File.writeTo (file, contents)
        end

    fun main () =
        let
            val args = Cli.getArgs (spec) handle
                Fail reason =>
                    (Format.printf "ERROR: %\n\n" [reason]; Cli.doHelp (spec); [])

            fun getNamed (flag) = Cli.getNamed (args, flag)

            val [backend] = getNamed (backendFlag)

            fun makeAbsolutePath (test) =
                Os.Path.join [Basis.OS.FileSys.getDir (), test]
            val tests = map makeAbsolutePath (testFiles "test")
            val testFile = generateFile (tests, backend)
            val testFileName = "/tmp/test.sml"
        in
            writeFile testFileName testFile;
            exec "ponyo-make" ([testFileName, "-I"] @ tests @ ["-b", backend, "-o", "/tmp/test"]);
            (* TODO: delete tmp files on success *)
            exec "/tmp/test" [];
            ()
        end

    end
end

val main = Main.main