structure Server =
struct
    type router = Request.t -> Response.t

    fun defaultRouteAct ()

    val defaultRouter = {act=defaultRouteAct}

    fun listenAndServe (address: string, port: int, router: router option) =
        
end
