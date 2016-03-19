signature F =
sig
    val f : int
    val t : string * string
    fun f : string -> string list
    fun d : (string * (string -> int)) -> (int * int)
end
