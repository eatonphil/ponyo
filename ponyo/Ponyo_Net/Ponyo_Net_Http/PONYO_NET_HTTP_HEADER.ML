signature PONYO_NET_HTTP_HEADER =
sig
    exception MalformedHeader of string
    type t
    val unmarshall : string -> t
    val marshall : t -> string
end
