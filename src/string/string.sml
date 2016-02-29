structure BasisString = String

structure String = 
struct
    exception IndexError of string * int

    fun all (source: string) (test: char -> bool) : bool =
        List.all test (explode (source))

    (* -capitalize: converts the first letter in the string to uppercase *)
    and capitalize (source: string) : string =
	case explode (source) of
	    [] => ""
	  | head :: tail => implode (Char.toUpper (head) :: tail)

    (* -charAt: gets the character at an index of a string *)
    and charAt (source: string, index: int) : char =
        if index < 0 orelse index > length (source)
	    then raise IndexError (source, index)
	else List.nth (explode source, index)

    (* -explode: converts a string to a char list *)
    and explode (source: string) : char list = BasisString.explode (source)

    (* -fromChar: converts the char to a string *)
    and fromChar (source: char) : string = BasisString.str (source)

    (* -hasPrefix: *)
    and hasPrefix (source: string, prefix: string) : bool =
        BasisString.isPrefix prefix source

    and hasSubstring (source: string, substring: string) : bool =
        BasisString.isSubstring substring source

    and hasSuffix (source: string, suffix: string) : bool =
        BasisString.isSuffix suffix source

    (* -implode: converts a char list to a string *)
    and implode (sourceList: char list) : string = BasisString.implode (sourceList)

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
    	BasisString.concatWith glue sources

        (* -length: gets the length of the string*)
    and length (source: string) : int = BasisString.size (source)

    (* -map: converts the source string to another string  *)
    and map (source: string) (func: char -> char) : string =
        BasisString.map func source

    (* TODO: implement *)
    and replace (source: string, match: string, replacement: string) : string = source

    and reverse (source: string) : string =
    	implode (rev (explode source))

    and substring (source: string, start: int, stop: int) : string =
        Substring.string (Substring.substring (source, start, stop))

    and substringToEnd (source: string, start: int) : string =
        Substring.string (Substring.extract (source, start, NONE))

    and split (source: string, delimiter: string) : string list =
        if isChar (delimiter) then
	    let
	        val delimChar = toChar (delimiter);
	        fun comp (c: char) : bool = (c = delimChar)
	    in
	        BasisString.tokens comp source
	    end
	else
	    let
	        fun split (s: string, i: int) : string * string =
		    let val (hd, tl) = Substring.splitAt (Substring.full s, i) in
		        (Substring.string hd, Substring.string tl)
		    end

	        fun doSplit (s: string, sl: string list) : string list =
		    case indexOf (s, delimiter) of
		        ~1 => rev (if s = "" then sl else s :: sl)
		      | 0 => doSplit (substringToEnd (s, 1), sl)
		      | i => let
		              val (hd, tl) = split (s, i);
			      val tlWithoutDelim = substringToEnd (tl, length (delimiter))
			  in
		              doSplit (tlWithoutDelim, hd :: sl)
			  end
	    in
	        doSplit (source, [])
            end

    and toChar (source: string) : char = charAt (source, 0)

    and toLower (source: string) : string =
        map source Char.toLower

    and toTitle (source: string) : string =
        join ((List.map capitalize (split (source, " "))), " ")

    and toUpper (source: string) : string =
        map source Char.toUpper
end
