structure Ponyo_Os_Filesystem : PONYO_OS_FILESYSTEM =
struct
    local
        structure FileSys = Basis.OS.FileSys

        structure String = Ponyo_String
        structure Path = Ponyo_Os_Path
    in

    structure File = Ponyo_Os_Filesystem_File;

    (* -exists: returns true if the path given is a valid file, directory,
     *  or symbolic link.
     *)
    fun exists (path: string) : bool =
        FileSys.isDir (path) orelse
          FileSys.isLink (path) orelse
          FileSys.fileSize (path) > ~1 handle
            Basis.OS.SysErr _ => false

    (* -expand: expands paths beginning with a tilde if the 
     *  current HOME is set.
     *)
    fun expand (path: string) =
        if not (String.hasPrefix (path, "~/")) then path
        else case Basis.OS.Process.getEnv "HOME" of
            SOME home => Path.join [home, String.substringToEnd (path, 2)]
          | NONE => path

    fun makeDirectory (path: string) : unit =
        let
            val directory = Path.directory (path) 
        in
            if not (exists directory) then makeDirectory (directory) else ();
            Basis.OS.FileSys.mkDir (path)
        end

    (* TODO: better name? *)
    fun walkWith (root: string) (walkFun: string -> 'a -> 'a) (init: 'a) : 'a =
        let
            fun withRoot (child: string) =
                Path.join [root, child]

            fun walkRoot (rootDir: FileSys.dirstream) (accum: 'a) : 'a =
                case FileSys.readDir (rootDir) of
                    NONE => accum
                  | SOME path =>
                let
                    val newAccum = walkFun (withRoot path) accum
                in
                    if FileSys.isDir (withRoot path)
                        then walkWith (withRoot path) walkFun newAccum
                    else newAccum;
                    walkRoot rootDir newAccum
                end
        in
            if FileSys.isDir (root)
               then walkRoot (Basis.OS.FileSys.openDir root) init handle
                   Basis.OS.SysErr _ => init
            else init
        end

    fun walk (root: string) (walkFun: string -> unit) : unit =
        walkWith root (fn a => (fn () => walkFun a)) ()

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
            case Basis.OS.Process.getEnv "PATH" of
                NONE => NONE
              | SOME paths => checkPaths (splitPaths (paths))
        end

    end
end
