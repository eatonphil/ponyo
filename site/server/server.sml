structure Main =
struct
    local
        structure Request = Ponyo.Net.Http.Request
        structure Response = Ponyo.Net.Http.Response
        structure Server = Ponyo.Net.Http.Server

        structure File = Ponyo.Os.FileSystem.File
        structure Path = Ponyo.Os.Path

        structure Format = Ponyo.Format
        structure String = Ponyo.String

        structure StringMap = Ponyo_Container_Map (Ponyo.String)
    in
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

        fun serveDocumentation (path: string) : Response.t =
            let
                val file = case String.substringToEnd (path, String.length "/documentation/") of
                    "string" => "Ponyo_String_/PONYO_STRING"
                  | "container/list" => "Ponyo_Container/PONYO_CONTAINER_LIST"
                  | "container/map" => "Ponyo_Container/PONYO_CONTAINER_MAP"
                  | "os/path" => "Ponyo_Os/PONYO_OS_PATH"
                  | "format" => "PONYO_FORMAT"
                  | _ => ""

                val fullPath = Path.join ["./dist/templates/documentation/ponyo", file ^ ".html"]
                val _ = PolyML.print fullPath
            in
                if file = "" then Response.new ("404 not found")
                else serveFile (fullPath)
            end

        fun main () =
            Server.listenAndServe ("", 4334, (fn (req) =>
                case Request.path (req) of
                    "/" => serveFile "./dist/templates/index.html"
                  | "/downloads" => serveFile "./dist/templates/downloads.html"
                  | "/documentation" => serveFile "./dist/templates/documentation.html"
                  | "/tutorials" => serveFile "./dist/templates/tutorials.html"
                  | "/news" => serveFile "./dist/templates/news.html"
                  | "/news/ponyo-for-standard-ml" => serveFile "./dist/templates/news/ponyo-for-standard-ml.html"
                  | _ => serveDocumentation (Request.path req)
            ))
    end
end

val main = Main.main
