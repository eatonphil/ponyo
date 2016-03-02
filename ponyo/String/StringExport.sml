structure StringExport = 
struct
    type t = string

    exception IndexError of string * int

    val WS = [" ", "\t", "\r", "\n", "\v", "\f"]
    
    (* -all: Returns true if all characters in the source string pass
     *  the test.
     *)
    fun all (source: string) (test: char -> bool) : bool =
        List.all test (explode (source))

    (* -capitalize: Converts the first letter in the string to uppercase. *)
    and capitalize (source: string) : string =
	case explode (source) of
	    [] => ""
	  | head :: tail => implode (Char.toUpper (head) :: tail)

    (* -charAt: Gets the character at the index of the string. *)
    and charAt (source: string, index: int) : char =
        if index < 0 orelse index > length (source)
	    then raise IndexError (source, index)
	else
	    List.nth (explode source, index)

    (* -compare: Compares two strings according to character comparisons. *)
    and compare (vals: string * string) : order = Basis.String.compare (vals)

    (* -count: The number of times a substring occurs in the string.
     *
     *  Ex:
     *      count ("foo", "oo") = 1
     *      count ("fooo", "oo") = 1
     *)
    and count (source: string, substring: string) : int =
        let
	    fun offset (i: int) = i + length (substring)
	    fun doCount (source: string, count: int) : int =
	        case source of
		    "" => count
		  | str => let in case indexOf(source, substring) of
		          ~1 => count
		        | index => doCount (substringToEnd (source, offset (index)), count + 1)
		    end
	in
	    if length (substring) > length (source)
	        then 0
	    else
	        doCount (source, 0)
	end

    (* -explode: Converts a string to a char list.
     *
     *  Ex:
     *      explode ("foo") = [#"f", #"o", #"o"]
     *      explode ("") = []
     *)
    and explode (source: string) : char list = Basis.String.explode (source)

    (* -fromChar: Converts the char to a string. *)
    and fromChar (source: char) : string = Basis.String.str (source)

    (* -hasPrefix: Returns true if the source string starts with the prefix. *)
    and hasPrefix (source: string, prefix: string) : bool =
        Basis.String.isPrefix prefix source

    (* -hasSubstring: Returns true if the source string contains the substring. *)
    and hasSubstring (source: string, substring: string) : bool =
        Basis.String.isSubstring substring source

    (* -hasSuffix: Returns true if the source string ends with the suffix. *)
    and hasSuffix (source: string, suffix: string) : bool =
        Basis.String.isSuffix suffix source

    (* -implode: Converts a list of characters to a string. *)
    and implode (sourceList: char list) : string = Basis.String.implode (sourceList)

    (* -indexOfFrom: Finds the first index of the pattern literal in the source
     *  string starting at a given point. Returns -1 if the pattern is
     *  longer than the source, the start is after the end of the source,
     *  or the pattern has not been found.
     *)
    and indexOfFrom (source: string, pattern: string, start: int) : int =
        if not (hasSubstring (source, pattern)) orelse
	   length (pattern) > length (source) andalso
	   start > length (source) then ~1
	else if length (pattern) = 0 then 0
	else
	    let
                fun isMatch (i: int, j: int) : bool =
		    if j = length (pattern)
		        then true
		    else if charAt (source, i) = charAt (pattern, j)
		        then isMatch (i + 1, j + 1)
		    else false

	        fun find (i: int) : int =
	            if isMatch (i, 0)
		        then i
		    else if i < length (source) - length (pattern)
		        then find (i + 1)
		    else ~1
	    in
	        find (0)
	    end

    (* -indexOf: Finds the first index of the pattern literal in the source string. *)
    and indexOf (source: string, pattern: string) : int =
    	indexOfFrom (source, pattern, 0)

    (* -isAlphaNum: Returns true if all characters in the string are alpha-numeric. *)
    and isAlphaNum (source: string) : bool =
        all source Char.isAlphaNum

    (* -isChar: Returns true if the string is a character. *)
    and isChar (source: string) : bool =
        length (source) = 1

    (* -isDigit: Returns true if all characters in the string are digits.
     *
     *  Ex:
     *      isDigit ("234567") = true
     *      isDigit ("2.3456") = false
     *)
    and isDigit (source: string) : bool =
    	all source Char.isDigit

    (* -isLower: Returns true if all characters in the string are lowercase. *)
    and isLower (source: string) : bool =
        all source Char.isLower

    (* -isUpper: Returns true if all characters in the string are uppercase. *)
    and isUpper (source: string) : bool =
        all source Char.isUpper

    (* -join: Merges together a list of strings into one string joined by the glue.
     *
     *  Ex:
     *      join (["This", "is", "a", "sentence."], " ") = "This is a sentence."
     *)
    and join (sources: string list, glue: string) =
    	Basis.String.concatWith glue sources

    (* -length: Gets the length of the string. *)
    and length (source: string) : int = Basis.String.size (source)

    (* -map: Produces a new string by applying func to each character in the source
     *  string.
     *
     *  Ex:
     *      map "twelve" Char.toUpper = "TWELVE"
     *)
    and map (source: string) (func: char -> char) : string =
        Basis.String.map func source

    (* TODO: implement *)
    and replace (source: string, match: string, replacement: string) : string = source

    (* -reverse: Reverses the source.
     *
     *  Ex:
     *      reverse ("food") = "doof"
     *)
    and reverse (source: string) : string =
    	implode (rev (explode source))

    (* -splitN: Splits the source string at the delimiter at most n times. The
     *  delimiter is removed at each split.
     *
     *  Ex:
     *      splitN ("foo:bar:foo", ":", 1) = ["foo", "bar:foo"]
     *)
    and splitN (source: string, delim: string, n: int) : string list =
        if n = 0
	    then [source]
	else
	    let
	        fun split (s: string, i: int) : string * string =
		    let val (hd, tl) = Substring.splitAt (Substring.full s, i) in
		        (Substring.string hd, Substring.string tl)
		    end

                fun revAndOptAddCurrent (current: string, accum: string list) : string list =
		    rev (if current = "" then accum else current :: accum)

	        fun doSplit (s: string, sl: string list) : string list =
		    if n = List.length (sl) then revAndOptAddCurrent (s, sl)
		    else case indexOf (s, delim) of
		        ~1 => revAndOptAddCurrent(s, sl)
		      | 0 => doSplit (substringToEnd (s, 1), sl)
		      | i => let
		              val (hd, tl) = split (s, i);
			      val tlWithoutDelim = substringToEnd (tl, length (delim))
			  in
		              doSplit (tlWithoutDelim, hd :: sl)
			  end
	    in
	        doSplit (source, [])
            end

    (* -split: Splits the string at the delimiter for every occurence of the
     *  delimiter. The delimiter is removed at each split.
     *
     *  Ex:
     *      split ("foo:bar:nil") = ["foo", "bar", "nil"]
     *)
    and split (source: string, delim: string) : string list =
        splitN (source, delim, 1 + count (source, delim))

    (* -stripLeft: Strips the source string of the garbage literal on the
     *  left-hand side as many times as the garbage literal appears on the
     *  left-hand side.
     *
     *  Ex:
     *      stripLeft ("  foo  ", " ") = "foo  "
     *)
    and stripLeft (source: string, garbage: string) : string =
        if hasPrefix (source, garbage)
	    then stripLeft (substringToEnd (source, length (garbage)), garbage)
	else
	    source

    (* -stripRight: Strips the source string of the garbage literal on the
     *  right-hand side as many times as the garbage literal appears on the
     *  right-hand side.
     *
     *  Ex:
     *      stripRight (" foo  ", " ") = " foo"
     *)
    and stripRight (source: string, garbage: string) : string =
        let val theEnd = length (source) - 1 in
	    if hasSuffix (source, garbage)
	        then stripRight (substring (source, 0, theEnd - length (garbage) + 1), garbage)
	    else
	        source
        end

    (* -strip: Strips the source string of the garbage literal on both the left
     *  and right sides as many times as the garbage literal appears on either side.
     *
     *  Ex:
     *      strip ("  foo   ", " ") = "foo"
     *)
    and strip (source: string, garbage: string) : string =
        stripRight (stripLeft (source, garbage), garbage)

    (* -stripAll: Strips the source string of all the garbage literals on both
     *  the left and right sides as many times as any of the garbage literals
     *  appear on the left or right side.
     *
     *  Ex:
     *      stripAll ("\t  \t f oo \t", ["\t", " "]) = "f oo"
     *)
    and stripAll (source: string, garbage: string list) : string =
        let
	    fun startsOrEndsWithGarbage (source: string, garbage: string list) : bool =
	        case garbage of
		    [] => false
		  | head :: tail => if hasPrefix (source, head) orelse
		                       hasSuffix (source, head)
                          then true
                      else startsOrEndsWithGarbage (source, tail)

            fun doStrip (garbage: string, stripped: string) =
	        strip (stripped, garbage)
	in
	    if startsOrEndsWithGarbage (source, garbage) then
		stripAll(foldl doStrip source garbage, garbage)
	    else
		source
	end

    (* -stripWhitespace: Shortcut to strip all whitespace (\r, \t, \n, \v, \f, " ")
     *  from the left and right sides of the source string. *)
    and stripWhitespace (source: string) : string =
        stripAll (source, WS)

    (* -substring: Returns the substring of the source from start (inclusive)
     *  to stop (exclusive).
     *
     *  Ex:
     *      substring ("foobar", 0, 3) = "foo"
     *)
    and substring (source: string, start: int, stop: int) : string =
        Substring.string (Substring.substring (source, start, stop))

    (* -substringToEnd: Returns the substring of the source from start (inclusive)
     *  to the end of the source string (inclusive).
     *
     *  Ex:
     *      substringToEnd ("foobar", 3) = "bar"
     *)
    and substringToEnd (source: string, start: int) : string =
        Substring.string (Substring.extract (source, start, NONE))

    (* -toChar: Assumes the source string to be a single character string and
     *  returns the first character.
     *
     *  Ex:
     *      toChar ("f") = #"f"
     *)
    and toChar (source: string) : char = charAt (source, 0)

    (* -toLower: Converts the source string to all lowercase.
     *
     *  Ex:
     *      toLower ("FoOOobAr") = "foooobar"
     *)
    and toLower (source: string) : string =
        map source Char.toLower

    (* -toTitle: Capitalizes each letter of a string following a space.
     *
     *  Ex:
     *      toTitle ("this is a title") = "This Is A Title"
     *)
    and toTitle (source: string) : string =
        join ((List.map capitalize (split (source, " "))), " ")

    (* -toUpper: Capitalizes each character of the source string.
     *
     *  Ex:
     *      toUpper ("aBcdEfG") = "ABCDEFG"
     *)
    and toUpper (source: string) : string =
        map source Char.toUpper
end
