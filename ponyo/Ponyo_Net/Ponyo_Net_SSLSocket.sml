structure Ponyo_Net_SSLSocket : PONYO_NET_SOCKET =
struct
    local
        open CInterface
        val get = get_sym (PONYO_ROOT ^ "/ssl.so");
    in

    type sslSocket = vol
    type ('a, 'b) t = sslSocket * ('a, 'b) Socket.sock

    fun connect (domain: string, port: int) : (INetSock.inet, Socket.active Socket.stream) t =
        let
            fun socketToInt s =
                SysWord.toInt (Posix.FileSys.fdToWord (valOf (Posix.FileSys.iodToFD (Socket.ioDesc s))))
            val sslWrap = call1 (get "ssl_wrap") INT POINTER;
            val socket = Ponyo_Net_Socket.connect (domain, port);
        in
            (sslWrap (socketToInt socket): sslSocket, socket: (INetSock.inet, Socket.active Socket.stream) Socket.sock)
        end

    fun close (sslSocket: sslSocket, socket: ('a, 'b) Socket.sock) : unit =
        let
            val sslClose = call1 (get "ssl_close") POINTER VOID;
        in
            sslClose (sslSocket);
            Ponyo_Net_Socket.close (socket)
        end

    fun read (socket: sslSocket, _: ('a, Socket.active Socket.stream) Socket.sock) : Word8Vector.vector =
        let
            val howMuch = 4096;
            val buf = alloc howMuch Cchar;
            fun readBuf buf 0 = []
              | readBuf buf n = fromCchar buf :: readBuf (offset 1 Cchar buf) (n - 1) 
            
            val numRead = call3 (get "ssl_read") (POINTER, POINTER, INT) INT (socket, address buf, howMuch);
        in
            if numRead <= 0
                then Byte.stringToBytes ""
            else Byte.stringToBytes (CharVector.fromList (readBuf buf numRead))
        end

    fun write ((socket: sslSocket, _: ('a, Socket.active Socket.stream) Socket.sock), toWrite: Word8VectorSlice.slice) : int =
        call3 (get "ssl_write") (POINTER, STRING, INT) INT (socket, Byte.unpackStringVec toWrite, Word8VectorSlice.length toWrite)

    fun writeAll (socket: ('a, Socket.active Socket.stream) t, toWrite: Word8Vector.vector) : unit =
        let
	    val toWriteLen = Word8Vector.length (toWrite)
	    val howMuch = 4096
	    val written = ref 0
	    fun min (a, b) = if a < b then a else b
	in
	    while (!written < toWriteLen) do
	        let
		    val theEnd = SOME (min (howMuch, toWriteLen - !written))
		    val currentBytes = Word8VectorSlice.slice (toWrite, !written, theEnd)
		in
	            written := !written + (write (socket, currentBytes))
		end
        end

    end
end
