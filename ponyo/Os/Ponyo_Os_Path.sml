structure Ponyo_Os_Path =
struct
    local structure String = Ponyo_String in

    fun base (path: string) : string =
        Basis.Os.Path.base (path)

    (* TODO: implement stub *)
    fun clean (path: string) : string =
        let
        in
            path
        end

    fun directory (path: string) : string =
        Basis.Os.Path.dir (path)
 
    fun extension (path: string) : string =
        case Basis.Os.Path.ext (clean path) of
            NONE => ""
          | SOME extension => extension

    (*
     * -file: File returns the filename portion of a file path.
     *
     *  Ex:
     *      file ("foo/bar.html") = "bar.html"
     *)
    fun file (path: string) : string =
        Basis.Os.Path.file (path)

    fun join (paths: string list) =
        case paths of
            [] => ""
          | [path] => path
          | path :: paths =>
             String.join ([String.stripRight (clean path, "/"),
                           String.stripLeft (join paths, "/")], "/")
    end
end
