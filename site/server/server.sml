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
                val sigPath = String.substringToEnd (path, String.length "/documentation/")
                val namespaces = map String.capitalize (String.split (sigPath, "/"))
                val folder = foldl
                    (fn (ns, path) => Path.join [path, ns])
                    (hd namespaces)
                    (tl namespaces)

                val file = String.toUpper (String.join ("ponyo" :: namespaces, "_")) ^ ".html"
                val fullPath = Path.join ["./dist/templates/documentation/ponyo", folder, file]
            in
                serveFile (fullPath)
            end

        fun main () =
            Server.listenAndServe ("", 4334, (fn (req) =>
                case Request.path (req) of
                    "/" => serveFile "./dist/templates/index.html"
                  | "/downloads" => serveFile "./dist/templates/downloads.html"
                  | "/documentation" => serveFile "./dist/templates/documentation.html"
                  | "/documentation/string" => serveDocumentation (Request.path req)
                  | "/documentation/container/list" => serveDocumentation (Request.path req)
                  | "/documentation/container/tree/binarysearch" => serveDocumentation (Request.path req)
                  | "/tutorials" => serveFile "./dist/templates/tutorials.html"
                  | "/news" => serveFile "./dist/templates/news.html"
                  | "/news/ponyo-for-standard-ml" => serveFile "./dist/templates/news/ponyo-for-standard-ml.html"
                  | _ => Response.new ("404 not found")
            ))
    end
end

val main = Main.main
