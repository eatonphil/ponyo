signature PONYO_OS_FILESYSTEM =
sig
    structure File : PONYO_OS_FILESYSTEM_FILE

    val exists : string -> bool
    val expand : string -> string
    val makeDirectory : string -> unit
    val walkWith : string -> (string -> 'a -> 'a) -> 'a -> 'a
    val walk : string -> (string -> unit) -> unit
    val which : string -> string option
end
