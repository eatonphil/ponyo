(* TODO: This should be moved out of net/http. Requests are not just http. *)
structure Request =
struct
    local structure String = Ponyo_String in

    exception MalformedRequest of string

    type t = {method  : Method.t,
	      path    : string,
              version : string,
	      headers : string Headers.t,
	      body    : string}

    fun method (request: t) = #method request
    fun path (request: t) = #path request
    fun version (request: t) = #version request
    fun headers (request: t) = #headers request
    fun body (request: t) = #body request

    fun new (method: Method.t, path: string, body: string) : t =
        let
            val path = if String.hasPrefix (path, "/") then path else "/" ^ path
	    fun toHeaders (hs): string Headers.t =
	        case hs of
		    [] => Headers.empty
		  | hd :: tl => Headers.insert (toHeaders tl) (Header.toKv hd)
        in
	    {
	        method  = method,
		path    = path,
                version = "HTTP/1.1",
		headers = toHeaders [Header.ContentLength (String.length body)],
		body    = body
	    }
	end

    fun parseFirstLine (line: string, request: Connection.t) : t =
        case String.split (line, " ") of
            [] => raise MalformedRequest (line)
          | list => if length (list) <> 3
                  then raise MalformedRequest (line)
              else {
                  method  = Method.fromString (List.nth (list, 0)),
                  path    = List.nth (list, 1),
                  version = List.nth (list, 2),
                  headers = #headers request,
                  body    = #body request
              }

    fun read (conn) : t =
        let
            val request = Connection.read (conn)
        in
            parseFirstLine (#firstLine request, request)
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

    fun write (conn, request) : unit =
        Connection.write (conn, marshall request)

    end
end
