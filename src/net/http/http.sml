structure HttpMethod =
struct
    datatype t =
        Connect
      | Delete
      | Get
      | Head
      | Options
      | Post
      | Put
      | Trace
      | Unknown of string

    fun fromString (v: string) : t = case String.toUpper(v) of
        "CONNECT" => Connect
      |	"DELETE"  => Delete
      | "GET"     => Get
      | "HEAD"    => Head
      | "OPTIONS" => Options
      | "POST"    => Post
      | "PUT"     => Put
      | "TRACE"   => Trace
      | _         => Unknown (v)

    fun toString v = case v of
        Connect   => "CONNECT"
      | Delete    => "DELETE"
      | Get       => "GET"
      | Head      => "HEAD"
      | Options   => "OPTIONS"
      | Post      => "POST"
      | Put       => "PUT"
      | Trace     => "TRACE"
      | Unknown v => String.toUpper(v)
end

structure HttpHeader =
struct
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

    fun toString v = case v of
        Accept          v      => "Accept"
      | ContentLength   v      => "Content-Length"
      | ContentEncoding v      => "Content-Encoding"
      | ContentType     v      => "Content-Type"
      | Date            v      => "Date"
      | Expires         v      => "Expires"
      | Host            v      => "Host"
      | Referrer        v      => "Referer" (* Famous spelling error. *)
      | UserAgent       v      => "User-Agent"
      | Vary            v      => "Vary"
      | Unknown         (f, _) => f

    val LWS = [" ", "\t"]

    fun unmarshal (line: string) : t =
        case String.splitN (line, ":", 1) of
	    [] => raise HeaderMalformed ("no header present: " ^ line)
	  | badValue :: [] => raise HeaderMalformed (line)
	  | field :: (value :: _) => let
	          val cleanValue = String.stripAll (value, LWS)
		  val cleanField = String.toLower (field)
	      in
	          case field of
		      "accept"           => Accept (cleanValue)
		    | "content-length"   => ContentLength (valOf (Int.fromString (cleanValue)))
		    | "content-encoding" => ContentEncoding (cleanValue)
		    | "content-type"     => ContentType (cleanValue)
		    | "date"             => Date (cleanValue)
		    | "expires"          => Expires (cleanValue)
		    | "host"             => Host (cleanValue)
		    | "referer"          => Referrer (cleanValue) (* Famous spelling error. *)
		    | "user-agent"       => UserAgent (cleanValue)
		    | "vary"             => Vary (cleanValue)
		    | unknown            => Unknown (field, cleanValue)
	      end

    fun marshal (v: t) : string =
        let
	    val value = case v of
                Accept          v      => v
	      | ContentLength   v      => Int.toString (v)
	      | ContentEncoding v      => v
	      | ContentType     v      => v
	      | Date            v      => v
	      | Expires         v      => v
	      | Host            v      => v
	      | Referrer        v      => v
	      | UserAgent       v      => v
	      | Vary            v      => v
	      | Unknown         (_, v) => v

            val key = toString (v)
	in
	    key ^ ": " ^ String.stripAll (value, LWS) 
	end
end

structure HttpHeaders =
struct
    type t = HttpHeader.t list
end

structure HttpRequest =
struct
    type t = {method  : HttpMethod.t,
	      domain  : string,
	      path    : string,
	      port    : int,
	      headers : HttpHeaders.t,
	      body    : string}

    fun marshal (request: t) : string =
        let
	    val method = HttpMethod.toString (#method request)
	    val path = #path request
	    val intro = method ^ " " ^ path ^ " HTTP/1.1\r\n"
	    val headers = List.foldl (fn (a, b) => a ^ "\r\n" ^ b) "" (List.map HttpHeader.marshal (#headers request))
	    val body = #body request
	in
	    intro ^ headers ^ "\r\n\r\n" ^ body
        end

    fun write (socket, request) =
        let
	    val bytes = Byte.stringToBytes (marshal (request))
	    val bytesLen = Word8Vector.length bytes
	    val toWrite = 4096
	    val written = ref 0
	    fun min (a, b) = if a < b then a else b
	in
	    while (!written < bytesLen) do
	        let
		    val currentBytes = Word8VectorSlice.slice (bytes, !written, SOME (min (toWrite, bytesLen - !written)))
		in
	            written := !written + (Socket.sendVec (socket, currentBytes))
		end
        end
end

structure HttpResponse =
struct
    exception MalformedResponse of string

    type complete = {version : string,
                     status  : int,
		     reason  : string,
                     headers : HttpHeaders.t,
	             body    : string}

    type incomplete = {response        : complete,
	               headersComplete : bool,
                       store           : string}

    type t = complete

    (* -parse: Takes a stream of strings and parses it line-by-line.
     *  If there is not a full line to parse, it will store the current
     *  line and return. It attempts to parse as many lines as it can.
     *)
    fun parse (response: incomplete, stream: string) : incomplete =
        let
	    val stream = (#store response) ^ stream;

            fun parseRequestLine (line: string) : (string * int * string) =
	        case String.split (line, " ") of
		    [] =>  raise MalformedResponse (line)
		  | list => if length (list) <> 3
		          then raise MalformedResponse (line)
	              else
		          (List.nth (list, 0),
			   valOf (Int.fromString (List.nth (list, 1))),
			   List.nth (list, 2))
		          

            (* -parseHeaderLine: Parses a header line and returns the header. *)
            fun parseHeaderLine (line: string) : HttpHeader.t option =
	        case String.split (line, ":") of
		      [] => NONE
		    | [badValue] => NONE
		    | header :: field => SOME (HttpHeader.unmarshal (line))

	    and doParse (stream: string, response: incomplete) : incomplete =
	        case String.indexOf (stream, "\r\n") of
		    (* No complete line to read yet. *)
		      ~1 => {
		            headersComplete = false,
			    response        = #response response,
			    store           = stream
		        }
	            (* Line was just read, a second \r\n is here, so body must be beginning. *)
		    | 0 => {
                            headersComplete = true,
			    store           = "",
			    response        = {
			        version = #version (#response response),
				status  = #status (#response response),
			        reason  = #reason (#response response),
                                headers = #headers (#response response),
				body    = String.substringToEnd(stream, 2)
                            }
                        }
	            (* Line can be read. *)
		    | j =>
		        let
		            val (line, stream) = case String.splitN (stream, "\r\n", 1) of
		            	[] => (stream, "") (* This case shouldn't be possible. *)
	                      | [stream] => (stream, "") (* Likewise shouldn't be possible. *)
		              | line :: (stream :: _) => (line, stream)

		    	    val response = if #status (#response response) = 0
			            then let val (v, s, r) = parseRequestLine (line) in {
					headersComplete = false,
					store           = "",
					response        = {
					    version = v,
					    status  = s,
					    reason  = r,
					    headers = [],
					    body    = ""
					}
                                    } end
			       else
			           case parseHeaderLine (line) of
				       (* Ignore any bad headers. *)
				       NONE => response
				     | SOME header => {
				           headersComplete = false,
					   store           = "",
					   response        = {
					       version     = #version (#response response),
					       status      = #status (#response response),
					       reason      = #reason (#response response),
					       headers     = header :: #headers (#response response),
					       body        = ""
					   }
                                       }
			in
			    doParse (stream, response)
			end
        in
	    if #headersComplete response
		then {
		    headersComplete = true,
		    store           = "",
		    response        = {
			version = #version (#response response),
		        status  = #status (#response response),
		        reason  = #reason (#response response),
		        headers = #headers (#response response),
		        body    = #body (#response response) ^ stream
	            }
		}
	    else
	        doParse (stream, response)
	end

    fun read (socket) =
        let
	    val toRead = 4096
	    val read = ref 0
	    val rsp = ref ""
	    val response : incomplete ref = ref {
                headersComplete = false,
		store           = "",
		response        = {
		    version = "",
		    status  = 0,
		    reason  = "",
                    headers = [],
		    body    = ""
                }
            }
	in
            while (!read > 0 orelse !rsp = "") do
	        let
		    val bytes = Socket.recvVec (socket, 4096)
		    val len = Word8Vector.length bytes
		in
		    read := (if len < toRead then 0 else 1);
		    rsp := !rsp ^ Byte.bytesToString (bytes);
		    if !rsp <> "" then response := parse (!response, !rsp) else ()
		end;
	    #response (!response)
        end
end

structure HttpServer =
struct
end

structure HttpClient =
struct
    exception E of string

    fun act (request: HttpRequest.t) : HttpResponse.t =
    	let
	    val domain = #domain request
	    val socket = INetSock.TCP.socket ()
	    val address =
	        let
		    val entry = case NetHostDB.getByName domain of
		        NONE => raise E "invalid domain"
		      | SOME entry => entry
		in
		    INetSock.toAddr (NetHostDB.addr entry, #port request)
		end;

            val _ = Socket.connect (socket, address)
	    val _ = HttpRequest.write (socket, request)
	    val rsp = HttpResponse.read (socket)
	in
	    rsp
        end
end

structure Http =
struct
    structure Request = HttpRequest

    structure Method = HttpMethod

    structure Header = HttpHeader

    structure Response = HttpResponse

    structure Client = HttpClient

    structure Server = HttpServer
end
