(*
 *  PONYO_NET_HTTP_SERVER: This is the default web server implementation
 *  provided by Ponyo. For a more in-depth example, check out the <a href="https://github.com/eatonphil/ponyo/blob/master/site/server/server.sml">source</a>
 *  of ponyo.org. The provided example will start a new HTTP server running
 *  at http://127.0.0.1:5000.
 *
 *  Example:
 *      Net.Server.listenAndServe "127.0.0.1" 5000
 *        (fn _ => Net.Response.init "Hello world!")
 *)
signature PONYO_NET_HTTP_SERVER =
sig
    structure Router : PONYO_NET_HTTP_ROUTER

    (*
     *  MAX_CONN: The max number of connections allowed to be open at once.
     *  Defaults to -1 (or as many concurrent connections as the system allows.)
     *)
    val MAX_CONN : int ref

    (*
     *  listenAndServe: This is the entrypoint to create and start an HTTP
     *  server. The first two arguments are the address and port to bind
     *  the server. The final argument is any function that takes a 
     *  Net.Request.t and returns a Net.Response.t. In addition to the most
     *  bare-bones Net.Router.t example above, Net.Router.basic is another
     *  slightly more useful implementation that maps routes and methods
     *  to functions that can return a response.
     *)
    val listenAndServe : string -> int -> Router.t -> unit 
end
