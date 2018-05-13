signature PONYO_STRING =
sig
    type t = string

    exception IndexError of (string * int * int)

    (*
     *  all: This returns true if each character in the given string
     *  passes the given test.
     *
     *  Ex:
     *      all ("Ponyo", Char.isDigit)   = false
     *      all ("0x12", Char.isAlphaNum) = true
     *)
    val all : string -> (char -> bool) -> bool

    (*
     *  capitalize: This converts the first character of the
     *  string to uppercase and the rest of the characters to
     *  lowercase.
     *
     *  Ex:
     *      capitalize ("ponyo") = "Ponyo"
     *      capitalize ("pOnYo") = "Ponyo"
     *)
    val capitalize : string -> string

    (*
     *  charAt: This returns the character at the given index
     *  of the given string. If the index is below zero, the index
     *  is added to the length and this becomes the new index. This
     *  is repeated until the index is greater-than or equal-to zero.
     *  Attempting to get a character at an index greater than the
     *  length of the string results in an IndexError.
     *
     *  Ex:
     *      charAt ("ponyo.org", 0)   = #"p"
     *      charAt ("ponyo.org", 1)   = #"o"
     *      charAt ("ponyo.org", ~1)  = #"g"
     *      charAt ("ponyo.org", ~11) = #"r"
     *)
    val charAt : string * int -> char

    (*
     *  compare: This returns LESS, GREATER, or EQUAL by comparing
     *  each character of the given strings.
     *
     *  Ex:
     *      compare ("ponyo.org", "qq.com")     = GREATER
     *      compare ("ponyo.org", "reddit.com") = LESS
     *      compare ("ponyo.org", "ponyo.org")  = EQUAL
     *)
    val compare : string * string -> order

    (*
     *  count: This returns the number of times the given substring
     *  occurs in the given string.
     *
     *  Ex:
     *      count ("http://ponyo.org", ":")   = 1
     *      count ("http://ponyo.org", "//")  = 1
     *      count ("http://ponyo.org", "org") = 1
     *)
    val count : string * string -> int

    (*
     *  explode: This converts a string to a list of characters.
     *
     *  Ex:
     *      explode ("ponyo") = [#"p", #"o", #"n", #"y", #"o"]
     *)
    val explode : string -> char list

    (*
     *  fromChar: This converts a character to a string.
     *
     *  Ex:
     *      fromChar (#"p") = "p"
     *)
    val fromChar : char -> string

    (*
     *  hasPrefix: This returns true if the given string
     *  begins with the given substring.
     *
     *  Ex:
     *      hasPrefix ("http://www.google.com", "http://") = true
     *      hasPrefix ("http://www.google.com", "www")     = false
     *)
    val hasPrefix : string * string -> bool

    (*
     *  hasSubstring: This returns true if the given string
     *  contains the given substring.
     *
     *  Ex:
     *      hasSubstring ("foobar", "oba") = true
     *)
    val hasSubstring : string * string -> bool

    (*
     *  hasSuffix: This returns true if the given string ends with
     *  the given substring.
     *
     *  Ex:
     *      hasSuffix ("http://www.google.com", ".com") = true
     *      hasSuffix ("http://ponyo.org", ".com")      = false
     *)
    val hasSuffix : string * string -> bool

    (*
     *  implode: This converts a list of characters to a string.
     *
     *  Ex:
     *      implode [#"p", #"o", #"n", #"y", #"o"] = "ponyo"
     *)
    val implode : char list -> string

    (*
     *  indexOf: This returns the first index of the given substring
     *  in the given string or negative one if the substring does
     *  not exist.
     *
     *  Ex:
     *      indexOf ("http://google.com", ":")  = 4
     *      indexOf ("http://ponyo.org", "com") = ~1
     *)
    val indexOf : string * string -> int

    (*
     *  indexOfFrom: This returns the first index of the given substring
     *  in the given string or negative one if the substring does not
     *  exist and starts at the given index.
     *
     *  Ex:
     *      indexOfFrom ("http://google.com:80", ":", 5) = 17
     *      indexOfFrom ("http://google.com", "com", 14) = ~1
     *)
    val indexOfFrom : string * string * int -> int

    (*
     *  isAlphaNum: This returns true if all characters within the
     *  given string are alphanumeric.
     *
     *  Ex:
     *      isAlphaNum ("0x12") = true
     *      isAlphaNum ("abc!") = false
     *)
    val isAlphaNum : string -> bool

    (*
     *  isChar: This returns true if the string has only one character.
     *)
    val isChar : string -> bool

    (*
     *  isDigit: This returns true if all characters in the string are
     *  digits between zero and nine.
     *)
    val isDigit : string -> bool

    (*
     *  isLower: This returns true if all characters in the string are
     *  lowercase.
     *)
    val isLower : string -> bool

    (*
     *  isUpper: This returns true if all characters in the string are
     *  uppercase.
     *)
    val isUpper : string -> bool

    (*
     *  join: This concatenates each string in the given list by the
     *  given glue.
     *
     *  Ex:
     *      join (["ponyo", ".", "org"], "") = "ponyo.org"
     *)
    val join : string list * string -> string

    (*
     *  length: This returns the number of characters in the string.
     *)
    val length : string -> int

    (*
     *  map: This returns a new string from the given string by
     *  converting each character of the given string by the given
     *  conversion.
     *
     *  Ex:
     *      map ("ponyo.org", (fn c => "-")) = "---------"
     *)
    val map : string -> (char -> char) -> string

    (*
     *  replace: This returns a new string from the given string
     *  by replacing every instance of the first substring with
     *  the second.
     *
     *  Ex:
     *      replace ("structure C =\nstruct\end", "\n", " ") = "structure C = struct end"
     *)
    val replace : string * string * string -> string

    val reverse : string -> string

    (*
     *  splitN: This splits the first string by the second at most
     *  n times.
     *
     *  Ex:
     *      splitN ("http://www.ponyo.org", ".", 1) = ["http://www", "ponyo.org"]
     *      splitN ("http://www.ponyo.org", ".", 3) = ["http://www", "ponyo", "org"]
     *)
    val splitN : string * string * int -> string list

    (*
     *  split: This splits the first string by the second as many
     *  times as possible.
     *
     *  Ex:
     *      split ("ponyo.org", ".") = ["ponyo", "org"]
     *)
    val split : string * string -> string list

    (*
     *  stripLeft: This removes all occurences of the second string
     *  in the first string on the left-hand side.
     *
     *  Ex:
     *      stripLeft ("  ponyo.org  ", " ") = "ponyo.org  "
     *)
    val stripLeft : string * string -> string

    (*
     *  stripRight: This removes all occurences of the second string
     *  in the first string on the right-hand side.
     *
     *  Ex:
     *      stripRight ("  ponyo.org  ", " ") = "  ponyo.org"
     *)
    val stripRight : string * string -> string

    (*
     *  strip: This removes all occurences of the second string in
     *  the first string on both sides.
     *
     *  Ex:
     *      strip ("  ponyo.org  ", " ") = "ponyo.org"
     *)
    val strip : string * string -> string

    (*
     *  stripAll: This removes all occurences of any of the given substrings
     *  in the given string as many times as possible.
     *
     *  Ex:
     *      stripAll ("  \t ponyo.org \n", [" ", "\t", "\n"]) = "ponyo.org"
     *)
    val stripAll : string * (string list) -> string

    (*
     *  stripWhitespace: This removes all occurences of white space from
     *  both sides of the string according to stripAll. Whitespace is defined
     *  by the WS value.
     *)
    val stripWhitespace : string -> string

    (*
     *  substring: This returns the section of the given string
     *  starting with the first index and includes the next n characters.
     *
     *  Ex:
     *      substring ("ponyo.org", 0, 1) = "p"
     *      substring ("ponyo.org", 2, 3) = "nyo"
     *)
    val substring : string * int * int -> string

    (*
     *  substringToEnd: This returns the section of the given string
     *  starting at the given index and ending at the end of the original
     *  string.
     *
     *  Ex:
     *      substringToEnd ("ponyo.org", 5) = ".org")
     *)
    val substringToEnd : string * int -> string

    (*
     *  toChar: This returns the first character of the string.
     *
     *  Ex:
     *      toChar ("p")         = #"p"
     *      toChar ("ponyo.org") = #"p"
     *)
    val toChar : string -> char

    (*
     *  toLower: This converts the given string to lowercase.
     *)
    val toLower : string -> string

    (*
     *  toTitle: This converts the first letter of each space-delimited
     *  substring of the given string to uppercase and the rest of the
     *  characters to lowercase.
     *
     *  Ex:
     *      toTitle ("ponyo is fun!") = "Ponyo Is Fun!"
     *      toTitle ("PONYO is FUN!") = "Ponyo Is Fun!"
     *)
    val toTitle : string -> string

    (* toUpper: This converts the given string to uppercase. *)
    val toUpper : string -> string

    val hash : string -> Word64.word
    val unitialized : string
    val toString : string -> string
end
