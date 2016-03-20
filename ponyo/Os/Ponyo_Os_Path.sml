structure Ponyo_Os_Path =
struct
    local structure String = Ponyo_String in

    fun join (paths: string list) =
        case paths of
            [] => ""
          | [path] => path
          | path :: paths =>
             String.join ([String.stripRight (path, "/"),
                           String.stripLeft (join paths, "/")], "/")
    end
end
