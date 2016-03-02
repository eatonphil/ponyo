structure Client =
struct
    exception InvalidHost of string

    fun act (request: Request.t) : Response.t =
    	let
	    val domain = #domain request
	    val socket = INetSock.TCP.socket ()
	    val address =
	        let
		    val entry = case NetHostDB.getByName (domain) of
		        NONE => raise InvalidHost (domain)
		      | SOME entry => entry
		in
		    INetSock.toAddr (NetHostDB.addr (entry), #port request)
		end;

            val _ = Socket.connect (socket, address)
	    val _ = Request.write (socket, request)
	    val rsp = Response.read (socket)
	in
	    rsp
        end
end
