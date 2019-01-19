signature PONYO_NET_HTTP_HEADER =
sig
    exception MalformedHeader of string
    type t
    val unmarshal : string -> t
    val marshal : t -> string
end
