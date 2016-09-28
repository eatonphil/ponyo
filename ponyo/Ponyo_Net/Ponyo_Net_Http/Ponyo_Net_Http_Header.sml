structure Ponyo_Net_Http_Header : PONYO_NET_HTTP_HEADER =
struct
    local structure String = Ponyo_String in

    exception MalformedHeader of string
    type t = string * string

    fun unmarshall (line: string) : string * string =
        case String.splitN (line, ":", 1) of
	    [] => raise MalformedHeader ("no header present: " ^ line)
	  | badValue :: [] => raise MalformedHeader (line)
	  | field :: (value :: _) => let
	          val cleanValue = String.stripWhitespace (value)
	      in
	          (String.toLower field, cleanValue)
	      end

    fun marshall (header: string, value: string) : string =
        header ^ ": " ^ String.stripWhitespace (value) 

    end
end
