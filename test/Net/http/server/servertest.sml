structure Server = Ponyo.Net.Http.Server
structure Format = Ponyo.Format

fun main () =
    Server.listenAndServe ("", 9339, (fn (req) =>
        Response.new (Format.sprintf "Hello world at %!" [Request.path req])
    ))
