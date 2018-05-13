structure Main =
struct
    local
        structure Method   = Ponyo.Net.Http.Method
        structure Request  = Ponyo.Net.Http.Request
        structure Response = Ponyo.Net.Http.Response
        structure Router   = Ponyo.Net.Http.Router
        structure Server   = Ponyo.Net.Http.Server

        structure Filesystem = Ponyo.Os.Filesystem
        structure File = Ponyo.Os.Filesystem.File
        structure Path = Ponyo.Os.Path

        structure Format = Ponyo.Format
        structure String = Ponyo.String
    in
        val fileRoot = "./dist/templates"
        val fileCache = ref (String.Dict.new ())

        fun serveFile (path: string) : Router.t =
            let
                val path = if path = "" then "" else Path.join [fileRoot, path]
                fun exists () = Filesystem.exists (path)
                fun getFile () = String.join (File.readFrom path, "")

                val newFile = ref true
                val file = case String.Dict.get (!fileCache) path of
                  NONE => if exists () then getFile () else "Page not found!"
                | SOME file => (newFile := false; file)
            in
                if !newFile
                    then fileCache := String.Dict.insert (!fileCache) path file
                else ();

                fn (_: Request.t) => Response.init (file)
            end

        fun serveDocumentation (request: Request.t) : Response.t =
            let
                val path = Request.path (request)
                val file = case String.substringToEnd (path, String.length "/documentation/") of
                    "string" => "String/String"
                  | "container/list" => "Container/List"
                  | "container/map" => "Container/Map"
                  | "os/path" => "Os/Path"
                  | "format" => "Format/Format"
                  | _ => ""

               val filePath = if file = ""
                   then ""
                   else Path.join ["documentation/src/", file ^ ".html"]
            in
                print (filePath ^ "\n");
                serveFile filePath request
            end

        fun serveHtml (request: Request.t) : Response.t =
            serveFile (Request.path request ^ ".html") request

        fun get (path: string, router: Router.t) : Method.t * string * Router.t =
            (Method.Get, path, router)

        fun main () =
            Server.listenAndServe "" 4334 (Router.basic [
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
