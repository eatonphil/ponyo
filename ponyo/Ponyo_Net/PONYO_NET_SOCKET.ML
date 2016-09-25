signature PONYO_NET_SOCKET =
sig
    type ('a, 'b) t
    val connect : string * int -> (INetSock.inet, Socket.active Socket.stream) t
    val close : ('a, 'b) t -> unit
    val read : ('a, Socket.active Socket.stream) t -> Word8Vector.vector
    val write : ('a, Socket.active Socket.stream) t * Word8VectorSlice.slice -> int
    val writeAll : ('a, Socket.active Socket.stream) t * Word8Vector.vector -> unit
end
