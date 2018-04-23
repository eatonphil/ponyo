structure Ponyo_Net_SSLSocket : PONYO_NET_SOCKET =
struct
    val bufferSize = 4096

    type sslSocket = MLton.Pointer.t
    type ('a, 'b) t = sslSocket * ('a, 'b) Socket.sock

    fun connect (domain: string, port: int) : (INetSock.inet, Socket.active Socket.stream) t =
        let
            fun socketToInt s =
                SysWord.toInt (Posix.FileSys.fdToWord (valOf (Posix.FileSys.iodToFD (Socket.ioDesc s))))
            val sslWrap = _import "ssl_wrap" public: Int.int -> sslSocket;
            val socket = Ponyo_Net_Socket.connect (domain, port)
        in
            (sslWrap (socketToInt socket): sslSocket, socket: (INetSock.inet, Socket.active Socket.stream) Socket.sock)
        end

    fun close (sslSocket: sslSocket, socket: ('a, 'b) Socket.sock) : unit =
        let
            val sslClose = _import "ssl_close" public: sslSocket -> unit;
        in
            sslClose (sslSocket);
            Ponyo_Net_Socket.close (socket)
        end

    fun read (socket: sslSocket, _: ('a, Socket.active Socket.stream) Socket.sock) : Word8Vector.vector =
        let
            val mem = Word8Array.array (bufferSize, 0w0)
            val sslRead = _import "ssl_read" public: (sslSocket * Word8Array.array * int) -> int;
            val numRead = sslRead (socket, mem, bufferSize)
        in
            if numRead <= 0
                then Byte.stringToBytes ""
            else
                Word8ArraySlice.vector (Word8ArraySlice.slice (mem, 0, SOME numRead))
        end

    fun write ((socket: sslSocket, _: ('a, Socket.active Socket.stream) Socket.sock), toWrite: Word8VectorSlice.slice) : int =
        let
            val sslWrite = _import "ssl_write" public: (sslSocket * string * int) -> int;
        in
            sslWrite (socket, Byte.unpackStringVec toWrite, Word8VectorSlice.length toWrite)
        end

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
