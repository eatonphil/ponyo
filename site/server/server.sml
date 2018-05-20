structure Main =
struct
    local
        structure Method = Ponyo.Net.Http.Method
        structure Mime = Ponyo.Net.Http.Mime
        structure Request = Ponyo.Net.Http.Request
        structure Response = Ponyo.Net.Http.Response
        structure Router = Ponyo.Net.Http.Router
        structure Server = Ponyo.Net.Http.Server

        structure Filesystem = Ponyo.Os.Filesystem
        structure File = Ponyo.Os.Filesystem.File
        structure Path = Ponyo.Os.Path

        structure Format = Ponyo.Format
        structure String = Ponyo.String
    in
        val fileRoot = "./dist"
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

                fn (_: Request.t) =>
                   let
                       val extension = Path.extension (path)
                       val mimetype = valOf (Mime.get Mime.types extension)
                       val headers = String.Dict.new ()
                       val headers = String.Dict.insert headers "Content-Type" mimetype
                       val rsp = Response.initWithHeaders file headers
                   in
                       rsp
                   end
            end

        fun serveReference (subsection: string) (request: Request.t) : Response.t =
            let
                val path = Request.path (request)
                val page = String.substringToEnd (path, String.length ("/reference/" ^ subsection ^ "/"))
                val mapped = if String.hasSubstring (page, "/") then page else page ^ "/" ^ page
                (* TODO: fix reference for Standard ML pages *)
                val filePath = Path.join ["reference/src/", mapped ^ ".html"]
            in
                Format.println ["Serving from path: ", filePath];
                serveFile (Path.join ["templates", filePath]) request
            end

        fun serveHtml (request: Request.t) : Response.t =
            serveFile ("templates" ^ Request.path request ^ ".html") request

        fun serveStatic (request: Request.t) : Response.t =
            serveFile (Request.path request) request

        fun get (path: string, router: Router.t) : Method.t * string * Router.t =
            (Method.Get, path, router)

        fun main () =
            Server.listenAndServe "" 4334 (Router.basic [
                get ("/", serveFile "templates/index.html"),
                get ("/reference", serveFile "templates/reference.html"),
                get ("/reference/ponyo/*", serveReference "ponyo"),
                get ("/reference/standardml/*", serveReference "standardml"),
                get ("/guides", serveFile "templates/guides.html"),
                get ("/guides/*", serveHtml),
                get ("/blog", serveFile "templates/blog.html"),
                get ("/blog/*", serveHtml),
                get ("/js/*", serveStatic),
                get ("/css/*", serveStatic)
            ])
    end
end

val main = Main.main
