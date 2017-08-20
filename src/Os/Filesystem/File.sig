signature PONYO_OS_FILESYSTEM_FILE =
sig
    exception NotReader
    exception NotWriter

    datatype mode = Read | Write | Append
    type t

    val close : t -> unit
    val use : string * mode -> t
    val read : t -> string list
    val readFrom : string -> string list
    val write : t * string -> unit
    val writeTo : string * string -> unit
end
