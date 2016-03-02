(* TODO: This should be moved out of net/http. Requests are not just http. *)
structure Request =
struct
    local structure String = StringExport in

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

    fun marshall (request: t) : string =
        let
	    val method = Method.toString (#method request)
	    val path = #path request
	    val intro = method ^ " " ^ path ^ " HTTP/1.1\r\n"
	    val marshalled = map Header.marshall (Headers.toList (#headers request))
	    val headers = foldl (fn (a, b) => String.join ([a, b], "\r\n")) "" marshalled
	    val body = #body request
	in
	    intro ^ headers ^ "\r\n\r\n" ^ body
        end

    fun write (socket, request) : unit =
        let
	    val bytes = Byte.stringToBytes (marshall request)
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
end
