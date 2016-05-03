structure Main =
struct
    local
        structure Method   = Ponyo.Net.Http.Method
        structure Request  = Ponyo.Net.Http.Request
        structure Response = Ponyo.Net.Http.Response
        structure Router   = Ponyo.Net.Http.Router
        structure Server   = Ponyo.Net.Http.Server

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
                PolyML.print(path);

                fn (_: Request.t) => Response.new (file)
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

        fun serveHtml (request: Request.t) : Response.t =
            serveFile (Request.path request ^ ".html") request

        fun get (path: string, router: Router.t) : Method.t * string * Router.t =
            (Method.Get, path, router)

        fun main () =
            Server.listenAndServe ("", 4334, Router.basic [
                get ("/",                serveFile "index.html"),
                get ("/downloads",       serveFile "downloads.html"),
                get ("/documentation",   serveFile "documentation.html"),
                get ("/documentation/*", serveDocumentation),
                get ("/handbook",        serveFile "handbook.html"),
                get ("/handbook/*",      serveHtml),
                get ("/news",            serveFile "news.html"),
                get ("/news/*",          serveHtml)
            ])
    end
end

val main = Main.main
