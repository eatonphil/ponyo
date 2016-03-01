structure Method =
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

structure Header =
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

infix 6 >>= >=>;

fun a >>= f = case a of
    NONE => NONE
  | SOME a => SOME (f a)

(* Inspired by Haskell reverse function composition: ">.>". *)
fun f >=> g = fn x => g (f x)

structure Headers =
struct
    structure HeadersBst = BinarySearchTree(String);

    open HeadersBst;

    fun get (hs: string HeadersBst.t) (h: string) : Header.t option =
        HeadersBst.get hs h >>= (fn v => Header.fromKv (h, v))

    fun toList (hs: string HeadersBst.t) : Header.t list =
	(HeadersBst.toList >=> map Header.fromKv) hs
end

(* TODO: This should be moved out of net/http. Requests are not just http. *)
structure Request =
struct
    type t = {method  : Method.t,
	      domain  : string,
	      path    : string,
	      port    : int,
	      headers : string Headers.t,
	      body    : string}

    fun new (method: Method.t, uri: string, body: string) : t =
        let
	    val uriSplit = String.split (uri, "/")
	    val hasScheme = String.hasSubstring (uri, "://")
	    val domainIndex = if hasScheme then 1 else 0
	    val domain = List.nth (uriSplit, domainIndex)
	    val pathIndex = if length (uriSplit) > domainIndex + 1 then domainIndex + 1 else ~1
	    val path = if pathIndex < 0 then "/" else List.nth (uriSplit, pathIndex)

	    fun toHeaders (hs): string Headers.t =
	        case hs of
		    [] => Headers.empty
		  | hd :: tl => Headers.insert (toHeaders tl) (Header.toKv hd)
        in
	    {
	        method  = method,
		port    = 80,
		body    = body,
		domain  = domain,
		path    = path,
		headers = toHeaders [Header.ContentLength (String.length body),
		                     Header.Host (domain)]
	    }
	end

    fun marshal (request: t) : string =
        let
	    val method = Method.toString (#method request)
	    val path = #path request
	    val intro = method ^ " " ^ path ^ " HTTP/1.1\r\n"
	    val marshalled = (Headers.toList >=> map Header.marshall) (#headers request)
	    val headers = foldl (fn (a, b) => String.join ([a, b], "\r\n")) "" marshalled
	    val body = #body request
	in
	    intro ^ headers ^ "\r\n\r\n" ^ body
        end

    fun write (socket, request) : unit =
        let
	    val bytes = (marshal >=> Byte.stringToBytes) request
	    val bytesLen = Word8Vector.length (bytes)
	    val toWrite = 4096
	    val written = ref 0
	    fun min (a, b) = if a < b then a else b
	in
	    while (!written < bytesLen) do
	        let
		    val theEnd = SOME (min (toWrite, bytesLen - !written))
		    val currentBytes = Word8VectorSlice.slice (bytes, !written, theEnd)
		in
	            written := !written + (Socket.sendVec (socket, currentBytes))
		end
        end
end

structure Response =
struct
    exception MalformedResponse of string

    type complete = {version : string,
                     status  : int,
		     reason  : string,
                     headers : string Headers.t,
	             body    : string}

    type incomplete = {response        : complete,
	               headersComplete : bool,
                       store           : string}

    type t = complete

    (* -parse: Takes a stream of strings and parses it line-by-line.
     *  If there is not a full line to parse, it will store the current
     *  line and return. It attempts to parse as many lines as it can.
     *)
    fun parse (response as {response=rspBody, ...}: incomplete, stream: string) : incomplete =
        let
	    val stream = (#store response) ^ stream;

            fun parseRequestLine (line: string) : (string * int * string) =
	        case String.split (line, " ") of
		    [] =>  raise MalformedResponse (line)
		  | list => if length (list) <> 3
		          then raise MalformedResponse (line)
	              else
		          (List.nth (list, 0),
			   (List.nth >=> Int.fromString >=> valOf) (list, 1),
			   List.nth (list, 2))
		          

            (* -parseHeaderLine: Parses a header line and returns the header. *)
            fun parseHeaderLine (line: string) : Header.t option =
	        case String.split (line, ":") of
		      [] => NONE
		    | [badValue] => NONE
		    | header :: field => SOME (Header.unmarshall (line))

	    and doParse (stream: string, response as {response=rspBody, ...}: incomplete) : incomplete =
	        case String.indexOf (stream, "\r\n") of
		    (* No complete line to read yet. *)
		      ~1 => { 
		            headersComplete = false,
			    response        = rspBody,
			    store           = stream
		        }
	            (* Line was just read, a second \r\n is here, so body must be beginning. *)
		    | 0 => {
                            headersComplete = true,
			    store           = "",
			    response        = {
			        version = #version rspBody, 
				status  = #status rspBody,
			        reason  = #reason rspBody,
                                headers = #headers rspBody,
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

		    	    val response = if #status rspBody = 0
			            then let val (v, s, r) = parseRequestLine (line) in {
					headersComplete = false,
					store           = "",
					response        = {
					    version = v,
					    status  = s,
					    reason  = r,
					    headers = Headers.empty,
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
					       version     = #version rspBody,
					       status      = #status rspBody,
					       reason      = #reason rspBody,
					       headers     = Headers.insert (#headers rspBody) (Header.toKv (header)),
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
			version = #version rspBody,
		        status  = #status rspBody,
		        reason  = #reason rspBody,
		        headers = #headers rspBody,
		        body    = #body rspBody ^ stream
	            }
		}
	    else
	        doParse (stream, response)
	end

    fun read (socket) =
        let
	    val toRead = 4096
	    val response : incomplete = {
                headersComplete = false,
		store           = "",
		response        = {
		    version = "",
		    status  = 0,
		    reason  = "",
                    headers = Headers.empty,
		    body    = ""
                }
            }

            fun doRead (response: incomplete) : incomplete =
	        let
		    val bytes = Socket.recvVec (socket, toRead)
		    val len = Word8Vector.length bytes
		    val read = Byte.bytesToString (bytes)
                    val response = parse (response, read)

                    val rspBody = (#response response)
		    val cl = Header.toString (Header.ContentLength 0)
		    val clValue = Headers.get (#headers rspBody) cl
		    val clPresent = clValue <> NONE
		    fun getCl () : int = case valOf (clValue) of
		        Header.ContentLength i => i
		      | _ => 0

                    val bodyLength = String.length (#body rspBody)
		    val readMore =
		        if not (#headersComplete response) then true
			else if clPresent andalso getCl () > bodyLength then true
			else false
		in
		    if readMore then doRead (response) else response
		end;
	in
	    #response (doRead (response))
        end
end

structure Server =
struct
end

structure Client =
struct
    exception InvalidHost of string

    fun act (request: Request.t) : Response.t =
    	let
	    val domain = #domain request
	    val socket = INetSock.TCP.socket ()
	    val address =
	        let
		    val entry = case NetHostDB.getByName (domain) of
		        NONE => raise InvalidHost (domain)
		      | SOME entry => entry
		in
		    INetSock.toAddr (NetHostDB.addr (entry), #port request)
		end;

            val _ = Socket.connect (socket, address)
	    val _ = Request.write (socket, request)
	    val rsp = Response.read (socket)
	in
	    rsp
        end
end

structure Http =
struct
    structure Request = Request

    structure Method = Method

    structure Header = Header

    structure Headers = Headers

    structure Response = Response

    structure Client = Client

    structure Server = Server
end
