structure Main =
struct
    local
        structure Request = Ponyo.Net.Http.Request
        structure Response = Ponyo.Net.Http.Response
        structure Router = Ponyo.Net.Http.Router
        structure Server = Ponyo.Net.Http.Server

        structure FileSystem = Ponyo.Os.FileSystem
        structure File = Ponyo.Os.FileSystem.File
        structure Path = Ponyo.Os.Path

        structure Format = Ponyo.Format
        structure String = Ponyo.String

        structure StringMap = Ponyo_Container_Map (Ponyo.String)
    in
        val fileRoot = "./dist/templates"
        val fileCache = ref StringMap.empty

        fun serveFile (path: string) : Router.t =
            let
                val path = if path = "" then "" else Path.join [fileRoot, path]
                fun exists () = FileSystem.exists (path)
                fun getFile () = String.join (File.readFrom path, "")

                val newFile = ref true
                val file = case StringMap.get (!fileCache) path of
                  NONE => if exists () then getFile () else "404 not found"
                | SOME file => (newFile := false; file)
            in
                if !newFile
                    then fileCache := StringMap.insert (!fileCache) (path, file)
                else ();

                fn (_: Request.t) => Response.new (file)
            end

        fun serveHandbook (request: Request.t) : Response.t =
            let
                val fullPath = (Request.path request) ^ ".html"
            in
                serveFile fullPath request
            end

        fun serveDocumentation (request: Request.t) : Response.t =
            let
                val path = Request.path (request)
                val file = case String.substringToEnd (path, String.length "/documentation/") of
                    "string" => "Ponyo_String_/PONYO_STRING"
                  | "container/list" => "Ponyo_Container/PONYO_CONTAINER_LIST"
                  | "container/map" => "Ponyo_Container/PONYO_CONTAINER_MAP"
                  | "os/path" => "Ponyo_Os/PONYO_OS_PATH"
                  | "format" => "PONYO_FORMAT"
                  | _ => ""

               val filePath = if file = ""
                   then ""
                   else Path.join ["documentation/ponyo", file ^ ".html"]
            in
                  serveFile filePath request
            end

        fun main () =
            Server.listenAndServe ("", 4334, Router.basic [
                ("/",                serveFile "index.html"),
                ("/downloads",       serveFile "downloads.html"),
                ("/documentation",   serveFile "documentation.html"),
                ("/documentation/*", serveDocumentation),
                ("/handbook",        serveFile "handbook.html"),
                ("/handbook/*",      serveHandbook),
                ("/news",            serveFile "news.html")
            ])
    end
end

val main = Main.main
