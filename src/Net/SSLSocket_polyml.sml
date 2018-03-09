(* This is not thread-safe. See Isabelle/HOL's implementation of with_memory for details. *)
datatype 'a Exn = Value of 'a | Exn of exn;

fun withMemory (n: int) f =
    let
        val mem = Foreign.Memory.malloc (Word.fromInt n);
        val res = Value (f mem) handle (res) => (Exn res);
        val _ = Foreign.Memory.free (mem);
     in
         case res of
             Exn exn => raise (exn)
           | Value v => v
     end

structure Ponyo_Net_SSLSocket : PONYO_NET_SOCKET =
struct
    local
        val library = Foreign.loadLibrary (PONYO_ROOT ^ "/ssl.so");
        val get = Foreign.getSymbol library;
    in

    val bufferSize = 4096

    type sslSocket = Foreign.Memory.voidStar
    type ('a, 'b) t = sslSocket * ('a, 'b) Socket.sock

    fun connect (domain: string, port: int) : (INetSock.inet, Socket.active Socket.stream) t =
        let
            fun socketToInt s =
                SysWord.toInt (Posix.FileSys.fdToWord (valOf (Posix.FileSys.iodToFD (Socket.ioDesc s))))
            val sslWrap = Foreign.buildCall1 (get "ssl_wrap", Foreign.cInt, Foreign.cPointer);
            val socket = Ponyo_Net_Socket.connect (domain, port);
        in
            (sslWrap (socketToInt socket): sslSocket, socket: (INetSock.inet, Socket.active Socket.stream) Socket.sock)
        end

    fun close (sslSocket: sslSocket, socket: ('a, 'b) Socket.sock) : unit =
        let
            val sslClose = Foreign.buildCall1 (get "ssl_close", Foreign.cPointer, Foreign.cVoid);
        in
            sslClose (sslSocket);
            Ponyo_Net_Socket.close (socket)
        end

    fun read (socket: sslSocket, _: ('a, Socket.active Socket.stream) Socket.sock) : Word8Vector.vector =
        withMemory bufferSize (fn mem =>
            let
                fun readBuf buf 0 = []
                  | readBuf buf n = Foreign.Memory.get8 (buf, Word.fromInt 0) :: readBuf (Foreign.Memory.++ (buf: Foreign.Memory.voidStar, Word.fromInt 1)) (n - 1) 

                val symbol = get ("ssl_read")
                val argTypes = (Foreign.cPointer, Foreign.cPointer, Foreign.cInt)
                val retType = Foreign.cInt
                val f = Foreign.buildCall3 (symbol, argTypes, retType)
                val numRead = f (socket, mem, bufferSize);
            in
                if numRead <= 0
                    then Byte.stringToBytes ""
                else
                    Word8Vector.fromList (readBuf mem numRead)
            end)

    fun write ((socket: sslSocket, _: ('a, Socket.active Socket.stream) Socket.sock), toWrite: Word8VectorSlice.slice) : int =
        let
            val symbol = get ("ssl_write")
            val argTypes = (Foreign.cPointer, Foreign.cString, Foreign.cInt)
            val retType = Foreign.cInt
            val f = Foreign.buildCall3 (symbol, argTypes, retType)
        in
            f (socket, Byte.unpackStringVec toWrite, Word8VectorSlice.length toWrite)
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
end
