structure Ponyo_Net_Https_Client =
struct
    local
        structure Socket   = Ponyo_Net_SSLSocket
        structure Header   = Ponyo_Net_Http_Header
        structure Headers  = Ponyo_Net_Http_Headers
        structure Response = Ponyo_Net_Http_Response (Socket)
        structure Request  = Ponyo_Net_Http_Request (Socket)
    in

    fun act (domain: string, request: Request.t) : Response.t =
    	let
            val host = Header.toKv (Header.Host domain)
            val request = {
                method  = #method request,
                path    = #path request,
                version = #version request,
                headers = Headers.insert (#headers request) host,
                body    = #body request
            }
            val socket = Socket.connect (domain, 443);
        in
	    Request.write (socket, request);
	    Response.read (socket)
        end
    end
end
