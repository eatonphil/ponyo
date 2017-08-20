structure Ponyo_Net_Socket : PONYO_NET_SOCKET =
struct
    type ('a, 'b) t = ('a, 'b) Socket.sock

    exception InvalidHost of string

    val bufferSize = 4096

    fun connect (domain: string, port) : (INetSock.inet, Socket.active Socket.stream) t =
        let
            val socket = INetSock.TCP.socket ()
	    val address =
	        let
		    val entry = case NetHostDB.getByName (domain) of
		        NONE => raise InvalidHost (domain)
		      | SOME entry => entry
		in
		    INetSock.toAddr (NetHostDB.addr (entry), port)
		end
        in
            Socket.connect (socket, address);
            socket
        end

    fun close (socket: ('a, 'b) t) : unit =
        Socket.close (socket)

    fun read (socket: ('a, Socket.active Socket.stream) t) : Word8Vector.vector =
        Socket.recvVec (socket, bufferSize)

    fun write (socket: ('a, Socket.active Socket.stream) t, toWrite: Word8VectorSlice.slice) : int =
        Socket.sendVec (socket, toWrite)

    fun writeAll (socket: ('a, Socket.active Socket.stream) t, toWrite: Word8Vector.vector) : unit =
        let
	    val toWriteLen = Word8Vector.length (toWrite)
	    val written = ref 0
	    fun min (a, b) = if a < b then a else b
	in
	    while (!written < toWriteLen) do
	        let
		    val theEnd = SOME (min (bufferSize, toWriteLen - !written))
		    val currentBytes = Word8VectorSlice.slice (toWrite, !written, theEnd)
		in
	            written := !written + (write (socket, currentBytes))
		end
        end
end
