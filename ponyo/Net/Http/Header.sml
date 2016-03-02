structure Header =
struct
    local structure String = StringExport in

    exception HeaderMalformed of string

    datatype t =
        Accept          of string
      | ContentLength   of int
      | ContentEncoding of string
      | ContentType     of string
      | Date            of string
      | Expires         of string
      | Host            of string
      | Referrer        of string
      | UserAgent       of string
      | Vary            of string
      | Unknown         of string * string

    val LWS = [" ", "\t"]

    fun toKv (h: t) : string * string = case h of
        Accept          v      => ("Accept", v)
      | ContentLength   v      => ("Content-Length", Int.toString (v))
      | ContentEncoding v      => ("Content-Encoding", v)
      | ContentType     v      => ("Content-Type", v)
      | Date            v      => ("Date", v)
      | Expires         v      => ("Expires", v)
      | Host            v      => ("Host", v)
      | Referrer        v      => ("Referer", v) (* Famous spelling error. *)
      | UserAgent       v      => ("User-Agent", v)
      | Vary            v      => ("Vary", v)
      | Unknown         (f, v) => (f, v)

    fun fromKv (h: string, v: string) : t = case String.toLower (h) of
        "accept"           => Accept (v)
      | "content-length"   => ContentLength (valOf (Int.fromString (v)))
      | "content-encoding" => ContentEncoding (v)
      | "date"             => Date (v)
      | "expires"          => Expires (v)
      | "host"             => Host (v)
      | "referer"          => Referrer (v)
      | "user-agent"       => UserAgent (v)
      | "vary"             => Vary (v)
      | _                  => Unknown (h, v)

    fun toString (h: t) =
        let val (s, _) = toKv (h) in s end

    fun unmarshall (line: string) : t =
        case String.splitN (line, ":", 1) of
	    [] => raise HeaderMalformed ("no header present: " ^ line)
	  | badValue :: [] => raise HeaderMalformed (line)
	  | field :: (value :: _) => let
	          val cleanValue = String.stripAll (value, LWS)
	      in
	          fromKv (field, cleanValue)
	      end

    fun marshall (v: t) : string =
        let
	    val (h, v) = toKv (v)
	in
	    h ^ ": " ^ String.stripAll (v, LWS) 
	end

    end
end
