signature PONYO_OS_PATH =
sig
    val base : string -> string
    val clean : string -> string
    val directory : string -> string
    val extension : string -> string
    val file : string -> string
    val join : string list -> string
end
