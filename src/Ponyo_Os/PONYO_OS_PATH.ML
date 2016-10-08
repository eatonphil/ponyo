signature PONYO_OS_PATH =
sig
    val base : string -> string
    val clean : string -> string
    val directory : string -> string

    (* -extension: *)
    val extension : string -> string

    (*
     * -file: Returns the full name of the file including the extension.
     *
     *  Ex:
     *      file ("foo/bar.html") = "bar.html"
     *)
    val file : string -> string

    (*
     * -filename: Returns only the name of the file (excludes the extension).
     *)
    val filename : string -> string

    val join : string list -> string
end
