signature PONYO_NET_HTTP_REQUEST =
sig
    exception MalformedRequest of string

    type socket
    type t

    val method : t -> Ponyo_Net_Http_Method.t
    val path : t -> string
    val version : t -> string
    val headers : t -> string Ponyo_String_Map.t
    val body : t -> string

    val new : string Ponyo_String_Map.t * string -> t
    val read : socket -> t
    val write : socket * t -> unit

    val marshall : t -> string
end
