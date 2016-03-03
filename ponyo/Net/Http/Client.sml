structure Client =
struct
    exception InvalidHost of string

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
	    val socket = INetSock.TCP.socket ()
	    val address =
	        let
		    val entry = case NetHostDB.getByName (domain) of
		        NONE => raise InvalidHost (domain)
		      | SOME entry => entry
		in
		    INetSock.toAddr (NetHostDB.addr (entry), 80)
		end
        in
            Socket.connect (socket, address);
	    Request.write (socket, request);
	    Response.read (socket)
        end
end
