signature PONYO_NET_HTTP_RESPONSE =
sig
    exception MalformedResponse of string

    type socket
    type t

    val version : t -> string
    val status : t -> int
    val reason : t -> string
    val headers : t -> string Ponyo_String.Dict.t
    val body : t -> string

    val new : string -> int -> string -> string Ponyo_String.Dict.t -> string -> t
    val init : string -> t
    val initWithHeaders : string -> string Ponyo_String.Dict.t -> t

    val NotFound : t
    val MethodNotAllowed : t
    val Unauthorized : t
    val Forbidden : t
    val InternalServerError : t
    val NotImplemented : t
    val BadGateway : t
    val ServiceUnavailable : t
    val GatewayTimeout : t

    val read : socket -> t
    val write : socket -> t -> unit

    val toString : t -> string
end
