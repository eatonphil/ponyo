functor Ponyo_Net_Http_Response (Socket: PONYO_NET_SOCKET) : PONYO_NET_HTTP_RESPONSE =
struct
    local
        structure String = Ponyo_String
        structure Format = Ponyo_Format

        structure Header     = Ponyo_Net_Http_Header
        structure Headers    = String.Dict
        structure Connection = Ponyo_Net_Http_Connection (Socket)
    in

    exception MalformedResponse of string

    type socket = (INetSock.inet, Basis.Socket.active Basis.Socket.stream) Socket.t
    type complete = {version : string,
                     status  : int,
		     reason  : string,
                     headers : string Headers.t,
	             body    : string}

    type incomplete = {response        : complete,
	               headersComplete : bool,
                       store           : string}

    type t = complete

    fun version (response: t) = #version response
    fun status (response: t) = #status response
    fun reason (response: t) = #reason response
    fun headers (response: t) = #headers response
    fun body (response: t) = #body response

    fun new (version: string) (status: int) (reason: string) (headers: string Headers.t) (body: string) : t =
        { version = version,
          status  = status,
          reason  = reason,
          headers = headers,
          body    = body }

    fun initWithHeaders (body: string) (headers: string Headers.t) : t =
        new "HTTP/1.1" 200 "OK" headers body

    fun init (body: string) : t =
        let
            val contentLength = Int.toString (String.length body)
            val headers = Headers.insert (Headers.new ()) "Content-Length" contentLength
        in
            initWithHeaders body headers
        end

    fun makeGenericResponse (status: int) (reason: string) : t =
        let
            val r = init (Int.toString status ^ " " ^ reason)
        in
            new (version r) status reason (headers r) (body r)
        end

    val NotFound = makeGenericResponse 404 "Not Found"
    val MethodNotAllowed = makeGenericResponse 405 "Method Not Allowed"
    val Unauthorized = makeGenericResponse 401 "Unauthorized"
    val Forbidden = makeGenericResponse 403 "Forbidden"
    val InternalServerError = makeGenericResponse 500 "InternalServerError"
    val NotImplemented = makeGenericResponse 501 "NotImplemented"
    val BadGateway = makeGenericResponse 502 "Bad Gateway"
    val ServiceUnavailable = makeGenericResponse 503 "Service Unavailable"
    val GatewayTimeout = makeGenericResponse 504 "Gateway Timeout"

    fun parseFirstLine (line: string) (response: Connection.t) : t =
        case String.splitN (line, " ", 2) of
            [version, status, reason] =>
              {
                  version = version,
                  status  = valOf (Int.fromString status),
                  reason  = reason,
                  headers = #headers response,
                  body    = #body response
              }
          | _ =>  raise MalformedResponse (line)

    fun read (conn: socket) : t =
        let
            val response = Connection.read (conn)
        in
            parseFirstLine (#firstLine response) response
        end

    fun marshall (response: t) : string =
        let
            val version = #version response
            val status = Int.toString (#status response)
            val reason = #reason response
            val intro = Format.sprintf "% % %\r\n" [version, status, reason]
            val marshalled = map Header.marshall (Headers.toList (#headers response))
            val headers = if length marshalled > 1
                    then foldl (fn (a, b) => String.join ([a, b], "\r\n")) "" marshalled
                else hd marshalled
            val body = #body response
        in
            intro ^ headers ^ "\r\n\r\n" ^ body
        end

    fun write (conn: socket) (response: t) : unit =
        Connection.write (conn, marshall response)

    val toString = marshall

    end
end
