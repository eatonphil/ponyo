signature PONYO_NET_HTTP_CLIENT =
sig
    exception InvalidRequestAddress of string

    type request
    type response

    val go : Ponyo_Net_Http_Method.t * string * request option -> response
    val get : string * request option -> response
    val put : string * request option -> response
    val post : string * request option -> response
    val delete : string * request option -> response
    val head : string * request option -> response
    val options : string * request option -> response
    val trace : string * request option -> response
    val connect : string * request option -> response
end
