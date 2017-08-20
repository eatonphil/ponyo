signature PONYO_NET_HTTP_REQUEST =
sig
    exception MalformedRequest of string

    type socket
    type t

    val method : t -> Ponyo_Net_Http_Method.t
    val path : t -> string
    val version : t -> string
    val headers : t -> string Ponyo_String.Dict.t
    val body : t -> string

    val new : Ponyo_Net_Http_Method.t -> string -> string -> string Ponyo_String.Dict.t -> string -> t
    val init : string Ponyo_String.Dict.t -> string -> t

    val read : socket -> t
    val write : socket -> t -> unit

    val toString : t -> string
end
