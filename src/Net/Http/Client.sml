functor Ponyo_Net_Http_Client (
    structure Port : sig val port : int end
    structure Socket : PONYO_NET_SOCKET
) : PONYO_NET_HTTP_CLIENT =
struct
    local
        structure String   = Ponyo_String
        structure Header   = Ponyo_Net_Http_Header
        structure Headers  = String.Dict
        structure Method   = Ponyo_Net_Http_Method
        structure Response = Ponyo_Net_Http_Response (Socket)
        structure Request  = Ponyo_Net_Http_Request (Socket)
    in

    type request = Request.t
    type response = Response.t

    exception InvalidRequestAddress of string

    fun go (method: Method.t, address: string, request': request option) : response =
    	let
            val address = if String.hasSubstring (address, "://")
                then (List.nth (String.split (address, "://"), 1))
                else address
            val (domain, path) = case String.splitN (address, "/", 1) of
                [domain, path] => (domain, "/" ^ path)
              | _ => raise InvalidRequestAddress (address)
            val request = case request' of
                SOME request => request
              | _ => Request.init (Headers.new ()) ""
            val request = {
                method  = method,
                path    = path,
                headers = Headers.insert (Request.headers request) "Host" domain,
                version = Request.version request,
                body    = Request.body request
            }
            val socket = Socket.connect (domain, Port.port);
        in
	    Request.write socket request;
	    Response.read socket
        end

    fun get (address: string, request: request option) : response =
        go (Method.Get, address, request)

    fun post (address: string, request: request option) : response =
        go (Method.Post, address, request)

    fun put (address: string, request: request option) : response =
        go (Method.Put, address, request)

    fun delete (address: string, request: request option) : response =
        go (Method.Delete, address, request)

    fun head (address: string, request: request option) : response =
        go (Method.Head, address, request)

    fun options (address: string, request: request option) : response =
        go (Method.Options, address, request)

    fun trace (address: string, request: request option) : response =
        go (Method.Trace, address, request)

    fun connect (address: string, request: request option) : response =
        go (Method.Connect, address, request)

    end
end
