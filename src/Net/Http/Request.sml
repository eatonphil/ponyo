functor Ponyo_Net_Http_Request (Socket: PONYO_NET_SOCKET) : PONYO_NET_HTTP_REQUEST =
struct
    local
        structure String = Ponyo_String

        structure Method     = Ponyo_Net_Http_Method
        structure Header     = Ponyo_Net_Http_Header
        structure Headers    = String.Dict
        structure Connection = Ponyo_Net_Http_Connection (Socket);
    in

    exception MalformedRequest of string

    type socket = (INetSock.inet, Basis.Socket.active Basis.Socket.stream) Socket.t
    type t = { method  : Method.t,
	       path     : string,
               version : string,
	       headers : string Headers.t,
	       body    : string }

    fun method  (request: t) = #method request
    fun path    (request: t) = #path request
    fun version (request: t) = #version request
    fun headers (request: t) = #headers request
    fun body    (request: t) = #body request

    fun new (method: Method.t) (path: string) (version: string) (headers: string Headers.t) (body: string) =
        { method  = method,
          path    = path,
          version = version,
          headers = headers,
          body    = body }

    fun init (headers: string Headers.t) (body: string) : t =
        let
            val contentLength = Int.toString (String.length body)
            val headers = Headers.insert headers "Content-Length" contentLength
        in
            new (Method.Unknown "") "" "HTTP/1.1" headers body
        end

    fun parseFirstLine (line: string) (request: Connection.t) : t =
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

    fun read (socket: socket) : t =
        let
            val request = Connection.read (socket)
        in
            parseFirstLine (#firstLine request) request
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
	    intro ^ headers ^ "\r\n" ^ body
        end

    fun write (socket: socket) (request: t) : unit =
        Connection.write (socket, marshall request)

    val toString = marshall

    end
end
