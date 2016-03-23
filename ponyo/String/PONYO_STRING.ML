signature PONYO_STRING =
sig
    type t = string

    (*
     *  all: All returns true if each character in the given string
     *  passes the given test.
     *
     *  Ex:
     *      all ("foo", Char.isDigit)    = false
     *      all ("12a", Char.isAlphaNum) = true
     *)
    val all : string -> (char -> bool) -> bool

    (*
     *  capitalize: Capitalize converts the first character of every
     *  space-delimited substring of the input to uppercase and
     *  lowercases all other characters.
     *
     *  Ex:
     *      capitalize ("this sentence.") = "This Sentence."
     *      capitalize ("THIS SENTENCE.") = "This Sentence."
     *)
    val capitalize : string -> string

    (*
     *  charAt: CharAt returns the character at the given index
     *  of the given string. If the index is below zero, the index
     *  is added to the length and this becomes the new index. This
     *  is repeated until the index is greater-than or equal-to zero.
     *  Attempting to get a character at an index greater than the
     *  length of the string results in an IndexError.
     *
     *  Ex:
     *      charAt ("foobar", 0)  = #"f"
     *      charAt ("foobar", 1)  = #"o"
     *      charAt ("foobar", ~1) = #"r"
     *      charAt ("foobar", ~9) = #"b"
     *)
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
    val stripAll : string * (string list) -> string
    val stripWhitespace : string -> string
    val substring : string * int * int -> string
    val substringToEnd : string * int -> string
    val toChar : string -> char
    val toLower : string -> string
    val toTitle : string -> string
    val toUpper : string -> string
end
