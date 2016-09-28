signature PONYO_NET_HTTP_RESPONSE =
sig
    exception MalformedResponse of string

    type socket
    type t

    val version : t -> string
    val status : t -> int
    val reason : t -> string
    val headers : t -> string Ponyo_String_Map.t
    val body : t -> string

    val new : string -> t
    val read : socket -> t
    val write : socket * t -> unit
end
