structure HttpMethod =
struct
    datatype t = Get | Post | Put | Head | Delete
    fun toString v = case v of
        Get    => "GET"
      | Post   => "POST"
      | Put    => "PUT"
      | Head   => "HEAD"
      | Delete => "DELETE"
end

structure HttpHeader =
struct
    datatype t =
        Accept          of string
      | ContentLength   of int
      | ContentEncoding of string
      | ContentType     of string
      | Date            of string
      | Expires         of string
      | Host            of string
      | Referrer        of string
      | Status          of int
      | UserAgent       of string
      | Vary            of string

    fun toString v = case v of
        Accept          v => "accept"
      | ContentLength   v => "content-length"
      | ContentEncoding v => "content-encoding"
      | ContentType     v => "content-type"
      | Date            v => "date"
      | Expires         v => "expires"
      | Host            v => "host"
      | Referrer        v => "referer"
      | Status          v => "status"
      | UserAgent       v => "user-agent"
      | Vary            v => "vary"

    fun marshal (v: t) : string =
        let
	    val value = case v of
                Accept          v => v
	      | ContentLength   v => Int.toString (v)
	      | ContentEncoding v => v
	      | ContentType     v => v
	      | Date            v => v
	      | Expires         v => v
	      | Host            v => v
	      | Referrer        v => v
	      | Status          v => Int.toString (v)
	      | UserAgent       v => v
	      | Vary            v => v

            val key = toString (v)
	in
	    key ^ ": " ^ value
	end
end

structure HttpHeaders =
struct
    type t = HttpHeader.t list
end

structure HttpRequest =
struct
    type t = {method   : HttpMethod.t,
              domain   : string,
	      path     : string,
	      port     : int,
	      headers  : HttpHeaders.t,
	      body     : string}

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
    type t = string

    fun read (socket) =
        let
	    val toRead = 4096
	    val read = ref 0
	    val rsp = ref ""
	in
            while (!read > 0 orelse !rsp = "") do
	        let
		    val bytes = Socket.recvVec (socket, 4096)
		    val len = Word8Vector.length bytes
		in
		    read := (if len < toRead then 0 else 1);
		    rsp := !rsp ^ Byte.bytesToString (bytes)
		end;
	    !rsp
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
