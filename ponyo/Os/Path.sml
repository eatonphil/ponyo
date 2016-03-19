structure Path =
struct
    local structure String = StringExport in

    fun expandHome (path: string) =
        if not (String.hasPrefix (path, "~/")) then path
        else case Basis.Os.Process.getEnv "HOME" of
            SOME home => join ([home, String.substringToEnd (path, 2)])
          | NONE => path

    and join (paths: string list) =
        case paths of
            [] => ""
          | [path] => path
          | path :: paths =>
             String.join ([String.stripRight (expandHome path, "/"),
                           String.stripLeft (join paths, "/")], "/")

    end
end
