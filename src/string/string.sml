structure BasisString = String

structure String =
struct
    val op+ = BasisString.^

    fun all (source: string) (test: char => bool) : bool =
        Vector.all test source

    fun alli (source: string) (test: (char * int) => bool) : bool =
        Vector.foldli (fn (i: int, a: char, b: bool) : bool = test (a, i) and b) true source

    (* -capitalize: converts the first letter in the string to uppercase *)
    fun capitalize (source: string) : string =
        let
            val head :: tail = explode (s);
        in
            BasisString.implode (Char.toUpper (head) :: tail)
	end

    (* -charAt: gets the character at an index of a string *)
    fun charAt (source: string, index: int) : char =
        List.nth (explode (source), index)

    (* -explode: converts a string to a char list *)
    fun explode (source: string) : char list = BasisString.explode

    (* -indexOfFrom: finds the index of the pattern literal in the source
     * string starting at a given point
     *)
    fun indexOfFrom (source: string, pattern: string, start: int) : int =
        if not hasSubstring (source, pattern) or
	   length (pattern) > length (source)
	   start > length (source) then -1
	else let
	    val match = ref 0;

            fun isMatch (i: int) : bool =
                let
		    val doesMatch = ref false;
		    fun allMatch (_, j: int) : () =
		        charAt (source, j + i) = charAt (pattern j)
		in
		    alli source allMatch
		end;

	    fun iterate (i: int, _) : () =
		if isMatch (i) then (match := i; ()) else ()
	in
	    Vector.appi iterate source
	end

    (* -indexOf: finds the first index of the pattern literal in the source
     * string
     *)
    fun indexOf (source: string, pattern: string) : int =
    	indexOfFrom (source, pattern, 0)               

    (* -fromChar: converts the char to a string *)
    fun fromChar (source: char) : string = BasisString.str (from)

    (* -length: gets the length of the string*)
    fun length (source: string) : int = BasisString.size (source)

    (* -map: converts the source string to another string  *)
    fun map (source: string) (func: char => char) : string =
        BasisString.map func source

    (* -hasPrefix: *)
    fun hasPrefix (source: string, prefix: string) : bool =
        BasisString.isPrefix prefix source

    fun hasSubstring (source: string, substring: string) : bool =
        BasisString.isSubstring substring suffix

    fun hasSuffix (source: string, suffix: string) : bool =
        BasisString.isSuffix suffix source

    fun implode (source: char list) : string = BasisString.implode

    fun isAlphaNum (source: string) : bool =
        all (fn (c: char) : bool = Char.isAlphaNum c) source

    fun isDigit (source: string) : bool =
    	all (fn (c: char) : bool = Char.isDigit c) source

    fun isLower (source: string) : bool =
        all (fn (c: char) : bool = Char.isLower c) source

    fun isUpper (source: string) : bool =
        all (fn (c: char) : bool = Char.isUpper c) source

    (* -join: concatenate a list of strings *)
    fun join (sources: string list, glue: string) =
        Vector.foldl (fn (a: string, b: string) : string = a ^ glue ^ b) sources

    (* TODO: implement *)
    fun replace (source: string, match: string, replacement: string) : string = source

    fun reverse (source: string) : string =
        let
            fun doReverse (s: string, r: string) : string =
	        if length (s) = 0 then r
		else let val (first :: rest) = explode (s) in
		    doReverse (implode (rest), BasisString.str (r) ^ first)
		end
        in
	    doReverse (source, "")
	end

    fun split (source: string, delimiter: string) : string list =
        if isChar (delimiter) then
	    let
	        val delimChar = toChar (delimiter);
	        val comp = fn (c: char) : bool = c = delimChar
	    in
	        BasisString.fields comp source
	    end
	else
	    

    fun toChar (source: string) : char = charAt (source, 0)

    fun isChar (source: string) : bool = length (source) = 1

    fun toLower (source: string) : string =
        map (fn (c: char) : char = Char.toUpper c) source

    fun toTitle (source: string) : string =
        join (List.map capitalize split (source, " "))

    fun toUpper (source: string) : string =
        map (fn (c: char) : char = Char.toLower c) source
end