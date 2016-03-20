signature PONYO_STRING =
sig
    type t = string

    val all : string -> (char -> bool) -> bool
    val capitalize : string -> string
    val charAt : string * int -> char
    val compare : string * string -> order
    val count : string * string -> int
    val explode : string -> char list
    val fromChar : char -> string
    val hasPrefix : string * string -> bool
    val hasSubstring : string * string -> bool
    val hasSuffix : string * string -> bool
    val implode : char list -> string
    val indexOfFrom : string * string * int -> int
    val indexOf : string * string -> int
    val isAlphaNum : string -> bool
    val isChar : string -> bool
    val isDigit : string -> bool
    val isLower : string -> bool
    val isUpper : string -> bool
    val join : string list * string -> string
    val length : string -> int
    val map : string -> (char -> char) -> string
    val replace : string * string * string -> string
    val reverse : string -> string
    val splitN : string * string * int -> string list
    val split : string * string -> string list
    val stripLeft : string * string -> string
    val stripRight : string * string -> string
    val strip : string * string -> string
    val stripAll : string * string list -> string
    val stripWhitespace : string -> string
    val substring : string * int * int -> string
    val substringToEnd : string * int -> string
    val toChar : string -> char
    val toLower : string -> string
    val toTitle : string -> string
    val toUpper : string -> string
end
