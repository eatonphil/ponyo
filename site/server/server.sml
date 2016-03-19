structure Request = Ponyo.Net.Http.Request
structure Response = Ponyo.Net.Http.Response
structure Server = Ponyo.Net.Http.Server

structure File = Ponyo.Os.File

structure Format = Ponyo.Format

fun serveFile (path: string) : Response.t =
    Response.new (String.join (File.readFrom (path), ""))

fun main () =
    Server.listenAndServe ("", 4334, (fn (req) =>
        case Request.path (req) of
            "/" => serveFile "./dist/templates/index.html"
          | "/downloads" => serveFile "./dist/templates/downloads.html"
          | "/documentation" => serveFile "./dist/templates/documentation.html"
          | "/tutorials" => serveFile "./dist/templates/tutorials.html"
          | "/news" => serveFile "./dist/templates/news.html"
          | _ => Response.new ("404 not found")
    ))
