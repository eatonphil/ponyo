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

    fun parseFirstLine (line: string) ({ headers, body, ... }: Connection.t) : t =
        case String.split (line, " ") of
            [] => raise MalformedRequest (line)
          | list => if length (list) <> 3
                  then raise MalformedRequest (line)
              else {
                  method  = Method.fromString (List.nth (list, 0)),
                  path    = List.nth (list, 1),
                  version = List.nth (list, 2),
                  headers = headers,
                  body    = body
              }

    fun read (socket: socket) : t =
        let
            val request = Connection.read (socket)
        in
            parseFirstLine (#firstLine request) request
        end

    fun marshall ({ version, path, headers, body, method }: t) : string =
        let
	    val method = Method.toString (method)
	    val intro = method ^ " " ^ path ^ " " ^ version ^"\r\n"
	    val marshalled = map Header.marshall (Headers.toList headers)
	    val headers = foldl (fn (a, b) => String.join ([a, b], "\r\n")) "" marshalled
	in
	    intro ^ headers ^ "\r\n" ^ body
        end

    fun write (socket: socket) (request: t) : unit =
        Connection.write (socket, marshall request)

    val toString = marshall

    end
end
