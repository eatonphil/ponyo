structure Json =
struct
    type t =
        Null
      | Bool of bool
      | String of string
      | Number of string
      | Float of float
      | Int of int
      | List of t list
      | Object of (string * t) list

    exception FormatError

    fun surrounds (source: string, left: string, right: string) : bool =
        String.hasPrefix (source, left) andalse String.hasPrefix (source, right)

    fun stripWs = String.stripWhitespace

    fun unmarshallObject (stream: string) =
        let
	    val cleanStream = stripWs (stream)
	    fun unenclosedStream () =
	        String.substring (cleanStream, 1, String.length (cleanStream) - 2)

            fun unmarshallPair (pair: string) : (string * t) =
	        let
		    val cleanPair = stripWs (pair)
		    val key = unmarshallString(cleanPair)
		    val valString = String.substringToEnd (cleanPair, String.length (key))
		in
		    (stripWs (key), unmarshall (valString)) 
		end

            fun unmarshallPairs (pairs: string list, unmarshalled: (string * t) list) : t =
	        case pairs of
		    [] => unmarshalled
		  | hd :: tl => unmarshallPairs (tl, unmarshallPair (hd) :: unmarshalled)
		    
	in
	    unmarshall (unmarshallPairs (String.substring (source))
	end

    (* -unmarshall: Entry point for parsing generic JSON.*)
    and unmarshall (source: string) : t =
        let val cleanStream = String.stripWs stream in
            case String.charAt (cleanStream, 0) of
	    	 "{" => unmarshallObject (String.substring (stream))
	       | "[" => unmarshallList (String.substring (cleanStream
        end
end
