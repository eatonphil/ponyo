structure Ponyo_String :> PONYO_STRING = 
struct
    type t = string

    exception IndexError of string * int * int

    val unitialized = ""

    val WS = [" ", "\t", "\r", "\n", "\v", "\f"]
    
    fun all (source: string) (test: char -> bool) : bool =
        List.all test (explode (source))

    and capitalize (source: string) : string =
	case explode (source) of
	    [] => ""
	  | head :: tail => implode (Char.toUpper (head) :: List.map Char.toLower tail)

    and charAt (source: string, index: int) : char =
        if index < 0 then charAt (source, length source + index)
        else if index >= length (source)
	    then raise IndexError (source, index, length (source))
	else
	    Basis.String.sub (source, index)

    and compare (vals: string * string) : order = Basis.String.compare (vals)

    and count (source: string, substring: string) : int =
        let
	    fun offset (i: int) = i + length (substring)
	    fun doCount (source: string, count: int) : int =
	        case source of
		    ""  => count
		  | str =>
                case indexOf (source, substring) of
		    ~1 => count
		  | index => doCount (substringToEnd (source, offset (index)), count + 1)
	in
	    if length (substring) > length (source)
	        then 0
	    else
	        doCount (source, 0)
	end

    and explode (source: string) : char list = Basis.String.explode (source)

    and fromChar (source: char) : string = Basis.String.str (source)

    and hasPrefix (source: string, prefix: string) : bool =
        Basis.String.isPrefix prefix source

    and hasSubstring (source: string, substring: string) : bool =
        substring <> "" andalso Basis.String.isSubstring substring source

    and hasSuffix (source: string, suffix: string) : bool =
        Basis.String.isSuffix suffix source

    and implode (sourceList: char list) : string = Basis.String.implode (sourceList)

    and indexOfFrom (source: string, pattern: string, start: int) : int =
        (* KMP algorithm *)
        let
            val patternLength = length pattern
            val sourceLength = length source
            val shifts = Array.tabulate (patternLength + 1, (fn _ => 1));
            val shift = ref 1

            val _ = mapi pattern (fn (i, c) =>
                let in
                    while (!shift) <= i andalso charAt (pattern, i) <> charAt (pattern, i - (!shift)) do
                        shift := Array.sub (shifts, i - (!shift));
                    Array.update (shifts, i + 1, !shift);
                    c
                end)

            val start = ref 0
            val matchLength = ref 0
            fun search (sourceIndex: int) =
                if sourceIndex = sourceLength - 1 then
                    start := ~1
                else
                    let
                        val c = charAt (source, sourceIndex)
                    in
                        while (!matchLength) = patternLength orelse
                              (!matchLength) >= 0 andalso
                              charAt (pattern, !matchLength) <> c do
                            let in
                                start := (!start) + Array.sub (shifts, !matchLength);
                                matchLength := (!matchLength) - Array.sub (shifts, !matchLength)
                            end;
                        matchLength := (!matchLength) + 1;
                        if (!matchLength) = patternLength then
                            ()
                        else
                            search (sourceIndex + 1)
                    end
        in
            search (0);
            !start
        end

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

    and join (sources: string list, glue: string) =
    	Basis.String.concatWith glue sources

    and length (source: string) : int = Basis.String.size (source)

    and map (source: string) (func: char -> char) : string =
        Basis.String.map func source

    and mapi (source: string) (func: (int * char) -> char) : string =
        let
            val i = ref 0
        in
            map source (fn (c) =>
                let
                    val c = func (!i, c)
                in
                     i := (!i) + 1;
                     c
                end)
        end

    and replace (source: string, match: string, replacement: string) : string =
        join (split (source, match), replacement)

    and reverse (source: string) : string =
    	implode (rev (explode source))

    and splitN (source: string, delim: string, n: int) : string list =
        if n = 0
	    then [source]
	else
	    let
	        fun split (s: string, i: int) : string * string =
		    let
                        val (hd, tl) = Substring.splitAt (Substring.full s, i)
                    in
		        (Substring.string hd, Substring.string tl)
		    end

                fun revAndOptAddCurrent (current: string, accum: string list) : string list =
		    rev (if current = "" then accum else current :: accum)

	        fun doSplit (s: string, sl: string list) : string list =
		    if n = List.length (sl) then revAndOptAddCurrent (s, sl)
		    else case indexOf (s, delim) of
		        ~1 => revAndOptAddCurrent(s, sl)
		      | 0 => doSplit (substringToEnd (s, length delim), "" :: sl)
		      | i =>
                          let
		              val (hd, tl) = split (s, i)
			      val tlWithoutDelim = substringToEnd (tl, length (delim))
			  in
		              doSplit (tlWithoutDelim, hd :: sl)
			  end
	    in
	        doSplit (source, [])
            end

    and split (source: string, delim: string) : string list =
        splitN (source, delim, ~1)

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
        if stop < 0 then substring (source, start, length source + stop)
        else Substring.string (Substring.substring (source, start, stop))

    and substringToEnd (source: string, start: int) : string =
        if start < 0 then substringToEnd (source, length source + start)
	else Substring.string (Substring.extract (source, start, NONE))

    and toChar (source: string) : char = charAt (source, 0)

    and toLower (source: string) : string =
        map source Char.toLower

    and toTitle (source: string) : string =
        join ((List.map capitalize (split (source, " "))), " ")

    and toUpper (source: string) : string =
        map source Char.toUpper

    (* http://www.cse.yorku.ca/~oz/hash.html djb2 *)
    and hash2 (source: string) : Word64.word =
        let
            val hash = ref (Word64.fromInt 5381)
            val sourceRef = ref source

            val fiveAsWord = Word.fromInt 5
        in
            while length (!sourceRef) > 0 do
                let
                    val c = (Word64.fromInt (Char.ord (charAt (!sourceRef, 0))))
                in
                    hash := Word64.<< (!hash, fiveAsWord) + !hash + c;
                    sourceRef := substringToEnd (!sourceRef, 1)
                end;
            !hash
        end

    and hash (source: string) : Word64.word =
        let
            val zero = Word64.fromInt (0)
            val one = Word64.fromInt (1)
            val seven = Word.fromInt (7)
            val magic = Word64.fromInt (1000003)
            fun currentCharacter (i) = (Word64.fromInt (Char.ord (charAt (source, i))))

            val l = length source
            val c = ref (0)

            val x = ref (Word64.<< (currentCharacter 0, seven))
        in
            (*
             * Iterating over the characters slightly reduces the number of allocations
             * compared to mapping over the string.
             *)
            while !c < l - 1 do let in
                x := Word64.xorb (Word64.* (magic, !x), (currentCharacter (!c)));
                c := !c + 1
            end;
            Word64.xorb (!x, Word64.fromInt l)
        end
end