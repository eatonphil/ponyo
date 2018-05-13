functor Ponyo_Net_Http_Router (Socket: PONYO_NET_SOCKET) : PONYO_NET_HTTP_ROUTER =
struct
    local
        structure String = Ponyo_String
        structure Method = Ponyo_Net_Http_Method 

        structure Request  = Ponyo_Net_Http_Request (Socket)
        structure Response = Ponyo_Net_Http_Response (Socket)
    in

    type request = Request.t
    type response = Response.t

    type t = Request.t -> Response.t

    fun basic (routeList: (Method.t * string * t) list) : t =
        let
            fun construct (routeList, routes) =
                case routeList of
                    (method, route, handler) :: tl =>
                      construct (tl, String.Dict.insert routes route (method, handler))
                  | _ => routes
            val routes = construct (routeList, String.Dict.new ());

            (* Turn a path "/foo/bar" into "/foo/*". *)
            fun pathToSlashStar (path: string) : string =
                let
                    val split = String.split (path, "/")
                    val last = if String.hasSuffix (path, "*") then 2 else 1
                    val butLast = List.take (split, length split - last)
                    val joined = String.join (butLast, "/")
                    val prefix = if String.hasPrefix (joined, "/") then "" else "/"
                in
                    if length split = 1 then "/*"
                    else prefix ^ String.join ([joined, "*"], "/")
                end

            fun handler (request, path) =
                case String.Dict.get routes path of
                    SOME (method, routeHandler) =>
                      if Request.method request <> method
                          then Response.MethodNotAllowed
                      else routeHandler (request)
                  | NONE =>
                      if path = "/*" orelse path = "//*"
                          then Response.NotFound
                      else if String.hasSuffix (path, "/")
                          then handler (request, String.substring (path, 0, ~1))
                      else handler (request, pathToSlashStar path)
        in
            fn (request) => handler (request, Request.path request)
        end

    end
end
