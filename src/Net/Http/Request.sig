signature PONYO_NET_HTTP_REQUEST =
sig
    exception MalformedRequest of string

    type socket
    type t

    val new : Ponyo_Net_Http_Method.t -> string -> string -> string Ponyo_String.Dict.t -> string -> t
    val init : string Ponyo_String.Dict.t -> string -> t

    val read : socket -> t
    val write : socket -> t -> unit

    val toString : t -> string
end
