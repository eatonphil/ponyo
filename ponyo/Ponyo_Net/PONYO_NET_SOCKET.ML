(*
 *  PONYO_NET_SOCKET: This signature is currently implemented by two structures:
 *  Ponyo.Net.Socket and Ponyo.Net.SSLSocket. These structures provide a low-
 *  level network interface and are used by Ponyo.Net.Http.Client and
 *  Ponyo.Net.Http.Server.
 *)
signature PONYO_NET_SOCKET =
sig
    (*
     *  t: The generic socket type.
     *)
    type ('a, 'b) t

    (*
     *  bufferSize: The size of the buffer used in read and write calls.
     *)
    val bufferSize : int

    (*
     *  connect: Initiates a connection on a socket.
     *)
    val connect : string * int -> (INetSock.inet, Socket.active Socket.stream) t

    (*
     *  close: Closes a connection on a socket.
     *)
    val close : ('a, 'b) t -> unit

    (*
     *  read: Reads a vector of size bufferSize from a socket.
     *)
    val read : ('a, Socket.active Socket.stream) t -> Word8Vector.vector

    (*
     *  write: Writes a vector of size bufferSize to a socket returning the
     *  number of bytes written.
     *)
    val write : ('a, Socket.active Socket.stream) t * Word8VectorSlice.slice -> int

    (*
     *  writeAll: Writes entire vector to a socket.
     *)
    val writeAll : ('a, Socket.active Socket.stream) t * Word8Vector.vector -> unit
end
