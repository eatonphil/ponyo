structure Request = Ponyo.Net.Http.Request
structure Response = Ponyo.Net.Http.Response
structure Server = Ponyo.Net.Http.Server

structure File = Ponyo.Os.FileSystem.File

structure Format = Ponyo.Format

structure StringMap = Ponyo_Container_Map (Ponyo.String)

val fileCache = ref StringMap.empty

fun serveFile (path: string) : Response.t =
    let
        fun getFile () = String.join (File.readFrom path, "")

        val newFile = ref false
        val file = case StringMap.get (!fileCache) path of
            NONE => getFile ()
          | SOME file => (newFile := true; file)
    in
        if !newFile
            then fileCache := StringMap.insert (!fileCache) (path, file)
        else ();
        Response.new (file)
    end

fun main () =
    Server.listenAndServe ("", 4334, (fn (req) =>
        case Request.path (req) of
            "/" => serveFile "./dist/templates/index.html"
          | "/downloads" => serveFile "./dist/templates/downloads.html"
          | "/documentation" => serveFile "./dist/templates/documentation.html"
          | "/documentation/string" => serveFile "./dist/templates/documentation/String/PONYO_STRING.html"
          | "/tutorials" => serveFile "./dist/templates/tutorials.html"
          | "/news" => serveFile "./dist/templates/news.html"
          | "/news/ponyo-for-standard-ml" => serveFile "./dist/templates/news/ponyo-for-standard-ml.html"
          | _ => Response.new ("404 not found")
    ))
