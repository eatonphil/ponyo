signature PONYO_NET_HTTP_ROUTER =
sig
    type request
    type response
    type t = request -> response

    val basic : (Ponyo_Net_Http_Method.t * string * t) list -> t
end
