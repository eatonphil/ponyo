structure Response =
struct
    local structure String = StringExport in

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
			   valOf (Int.fromString (List.nth (list, 1))),
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
end
