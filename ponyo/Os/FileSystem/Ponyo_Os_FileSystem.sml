structure Ponyo_Os_FileSystem =
struct
    local
        structure FileSys = Basis.Os.FileSys

        structure String = Ponyo_String
        structure Path = Ponyo_Os_Path
    in

    (* -expand: expands paths beginning with a tilde if the 
     *  current HOME is set.
     *)
    fun expand (path: string) =
        if not (String.hasPrefix (path, "~/")) then path
        else case Basis.Os.Process.getEnv "HOME" of
            SOME home => Path.join [home, String.substringToEnd (path, 2)]
          | NONE => path

    fun walk (root: string, walkFun: string -> unit) : unit =
        let
            fun withRoot (child: string) =
                Path.join [root, child]

            fun walkRoot (rootDir: FileSys.dirstream) : unit =
                case FileSys.readDir (rootDir) of
                    NONE => ()
                  | SOME path =>
                let in
                    walkFun (withRoot path);
                    if FileSys.isDir (withRoot path)
                        then walk (withRoot path, walkFun)
                    else walkRoot (rootDir)
                end
        in
            if FileSys.isDir (root)
               then walkRoot (Basis.Os.FileSys.openDir (root)) handle
                   Basis.Os.SysErr _ => ()
            else ()
        end

    (* -exists: returns true if the path given is a valid file, directory,
     *  or symbolic link.
     *)
    fun exists (path: string) : bool =
        FileSys.isDir (path) orelse
          FileSys.isLink (path) orelse
          FileSys.fileSize (path) > ~1 handle
            Basis.Os.SysErr _ => false

    fun which (executable: string) : string option =
        let
            fun splitPaths (paths: string) : string list = String.split (paths, ":")

            val executablePath = ref ""

            fun checkPaths (paths: string list) : string option =
                case paths of
                    [] => NONE
                  | path :: paths =>
                let
                    val fullPath = Path.join [path, executable]
                in
                    if exists (fullPath) andalso not (FileSys.isDir fullPath)
                        then SOME fullPath
                    else checkPaths (paths)
                end
        in
            case Basis.Os.Process.getEnv "PATH" of
                NONE => NONE
              | SOME paths => checkPaths (splitPaths (paths))
        end

    end
end
