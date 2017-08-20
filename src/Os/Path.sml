structure Ponyo_Os_Path : PONYO_OS_PATH =
struct
    local structure String = Ponyo_String in

    fun base (path: string) : string =
        Basis.OS.Path.base (path)

    (* TODO: implement stub *)
    fun clean (path: string) : string =
        let
        in
            path
        end

    fun directory (path: string) : string =
        Basis.OS.Path.dir (path)
 
    fun extension (path: string) : string =
        case Basis.OS.Path.ext (clean path) of
            NONE => ""
          | SOME extension => extension

    fun file (path: string) : string =
        Basis.OS.Path.file (path)

    fun filename (path: string) : string =
        String.substring (file path, 0, ~1 * (1 + String.length (extension path)))

    fun join (paths: string list) =
        case paths of
            [] => ""
          | [path] => path
          | path :: paths =>
             String.join ([String.stripRight (clean path, "/"),
                           String.stripLeft (join paths, "/")], "/")
    end
end
