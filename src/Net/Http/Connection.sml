functor Ponyo_Net_Http_Connection (Socket: PONYO_NET_SOCKET) =
struct
    local
        structure String = Ponyo_String

        structure Header  = Ponyo_Net_Http_Header
        structure Headers = String.Dict
    in

    type complete = {firstLine : string,
                     headers   : string Headers.t,
	             body      : string}

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
            val rspBody = #response response
	    val stream = (#store response) ^ stream;

            (* -parseHeaderLine: Parses a header line and returns the header. *)
            fun parseHeaderLine (line: string) : Header.t option =
                case String.split (line, ":") of
                    [] => NONE
                  | [badValue] => NONE
                  | header :: field => SOME (Header.unmarshall (line))

	    fun doParse (stream: string, response as {response=rspBody, ...}: incomplete) : incomplete =
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
                                firstLine = #firstLine rspBody,
                                headers   = #headers rspBody,
				body      = String.substringToEnd(stream, 2)
                            }
                        }
	            (* Line can be read. *)
		    | j =>
		        let
		            val (line, stream) = case String.splitN (stream, "\r\n", 1) of
		            	[] => (stream, "") (* This case shouldn't be possible. *)
	                      | [stream] => (stream, "") (* Likewise shouldn't be possible. *)
		              | line :: (stream :: _) => (line, stream)

		    	    val response = if #firstLine rspBody = ""
			        then { 
				    headersComplete = false,
			                store           = "",
					response        = {
                                            firstLine = line,
					    headers   = Headers.new (),
					    body      = ""
					}
                                    }
			        else
			            case parseHeaderLine (line) of
				        (* Ignore any bad headers. *)
				        NONE => response
				      | SOME (header, value) => {
				          headersComplete = false,
					  store           = "",
					  response        = {
                                              firstLine = #firstLine rspBody,
					      headers   = Headers.insert (#headers rspBody) header value,
					      body      = ""
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
                        firstLine = #firstLine rspBody,
		        headers   = #headers rspBody,
		        body      = #body rspBody ^ stream
	            }
		}
	    else
	        doParse (stream, response)
	end

    fun read (conn: ('a, Basis.Socket.active Basis.Socket.stream) Socket.t) =
        let
	    val response : incomplete = {
                headersComplete = false,
		store           = "",
		response        = {
                    firstLine = "",
                    headers   = Headers.new (),
		    body      = ""
                }
            }

            fun doRead (response: incomplete) : incomplete =
	        let
                    val bytes = Socket.read (conn)
		    val len = Word8Vector.length bytes
		    val read = Byte.bytesToString (bytes)
                    val response = parse (response, read)

                    val rspBody = (#response response)
                    val bodyLength = String.length (#body rspBody)
		    val contentLength = Headers.get (#headers rspBody) "content-length"
		    val contentRemains = case contentLength of
                        NONE => false
		      | SOME contentLengthValue => case Int.fromString (contentLengthValue) of
                        SOME i => i >  bodyLength
                      | NONE => true

		    val readMore = not (#headersComplete response) orelse contentRemains
		in
		    if readMore then doRead (response) else response
		end;
	in
	    #response (doRead (response))
        end

    fun write (conn: ('a, Basis.Socket.active Basis.Socket.stream) Socket.t, output: string) : unit =
        let
	    val bytes = Byte.stringToBytes (output)
        in
            Socket.writeAll (conn, bytes)
        end

    end
end
