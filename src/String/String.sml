structure Ponyo_String_internal :> PONYO_STRING = 
struct
    type t = string

    exception IndexError of string * int * int

    val unitialized = ""

    val WS = [" ", "\t", "\r", "\n", "\v", "\f"]
    
    fun all (source: string) (test: char -> bool) : bool =
        List.all test (explode (source))

    (*
     * Iterating over the characters slightly reduces the number of allocations
     * compared to mapping over the string. Using a ref also slightly reduces the
     * number of allocations compared to using a recursive helper function.
     *)
    fun app (source: string) (f: char -> unit) : unit =
        let
            val l = length (source)
            val i = ref 0
        in
            while !i < l do (f (charAt (source, !i)); i := !i + 1)
        end

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
                 find (start)
             end
        (* TODO: use KMP algorithm
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
        end*)

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
		    rev (current :: accum)

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

    (* http://www.cse.yorku.ca/~oz/hash.html djb2 second version *)
    and djb2a (source: string) : Word64.word =
        let
            val five = Word.fromInt (5)
            val hash = ref (Word32.fromInt 5381)
        in
            app source (fn (c) =>
                hash := Word32.xorb ((Word32.<< (!hash, five)) + (!hash), (Word32.fromInt (Char.ord c))));
            Word32.toLargeWord (!hash)
        end

    (*
     * The Python 2 hashing algorithm for testing and reference.
     * https://stackoverflow.com/questions/2070276/where-can-i-find-source-or-algorithm-of-pythons-hash-function
     *)
    and python2 (source: string) : Word64.word =
        let
            val zero = Word32.fromInt (0)
            val one = Word32.fromInt (1)
            val seven = Word.fromInt (7)
            val magic = Word32.fromInt (1000003)
            fun charToWord (c) = Word32.fromInt (Char.ord c)

            val l = length source
            val x = ref (Word32.<< (charToWord (charAt (source, 0)), seven))
        in
            app source (fn c => x := Word32.xorb (Word32.* (magic, !x), charToWord c));
            Word32.toLargeWord (Word32.xorb (!x, Word32.fromInt l))
        end

    val hash = djb2a

    fun toString (s: string) : string = s
end
