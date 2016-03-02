structure StringExport = 
struct
    type t = string

    exception IndexError of string * int

    val WS = [" ", "\t", "\r", "\n", "\v", "\f"]
    
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

    (* -fromChar: converts the char to a string *)
    and fromChar (source: char) : string = Basis.String.str (source)

    (* -hasPrefix: *)
    and hasPrefix (source: string, prefix: string) : bool =
        Basis.String.isPrefix prefix source

    and hasSubstring (source: string, substring: string) : bool =
        Basis.String.isSubstring substring source

    and hasSuffix (source: string, suffix: string) : bool =
        Basis.String.isSuffix suffix source

    (* -implode: converts a char list to a string *)
    and implode (sourceList: char list) : string = Basis.String.implode (sourceList)

    (* -indexOfFrom: Finds the first index of the pattern literal in the source
     * string starting at a given point. Returns -1 if the pattern is
     * longer than the source, the start is after the end of the source,
     * or the pattern has not been found.
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

    (* -indexOf: finds the first index of the pattern literal in the source
     * string
     *)
    and indexOf (source: string, pattern: string) : int =
    	indexOfFrom (source, pattern, 0)

    and isAlphaNum (source: string) : bool =
        all source Char.isAlphaNum

    and isChar (source: string) : bool =
        length (source) = 1

    and isDigit (source: string) : bool =
    	all source Char.isDigit

    and isLower (source: string) : bool =
        all source Char.isLower

    and isUpper (source: string) : bool =
        all source Char.isUpper

    (* -join: concatenate a list of strings *)
    and join (sources: string list, glue: string) =
    	Basis.String.concatWith glue sources

        (* -length: gets the length of the string*)
    and length (source: string) : int = Basis.String.size (source)

    (* -map: converts the source string to another string  *)
    and map (source: string) (func: char -> char) : string =
        Basis.String.map func source

    (* TODO: implement *)
    and replace (source: string, match: string, replacement: string) : string = source

    and reverse (source: string) : string =
    	implode (rev (explode source))

    (* -splitN: splits the source string by the delimiter at most n times.
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

    and split (source: string, delim: string) : string list =
        splitN (source, delim, 1 + count (source, delim))

    and stripLeft (source: string, garbage: string) : string =
        if hasPrefix (source, garbage)
	    then stripLeft (substringToEnd (source, length (garbage)), garbage)
	else
	    source

    and stripRight (source: string, garbage: string) : string =
        let val theEnd = length (source) - 1 in
	    if hasSuffix (source, garbage)
	        then stripRight (substring (source, 0, theEnd - length (garbage) + 1), garbage)
	    else
	        source
        end

    and strip (source: string, garbage: string) : string =
        stripRight (stripLeft (source, garbage), garbage)

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
    
    and stripWhitespace (source: string) : string =
        stripAll (source, WS)

    and substring (source: string, start: int, stop: int) : string =
        Substring.string (Substring.substring (source, start, stop))

    and substringToEnd (source: string, start: int) : string =
        Substring.string (Substring.extract (source, start, NONE))

    and toChar (source: string) : char = charAt (source, 0)

    and toLower (source: string) : string =
        map source Char.toLower

    and toTitle (source: string) : string =
        join ((List.map capitalize (split (source, " "))), " ")

    and toUpper (source: string) : string =
        map source Char.toUpper
end
