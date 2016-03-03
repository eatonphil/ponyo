structure Response =
struct
    local
        structure String = StringExport
        structure Format = FormatExport
    in

    exception MalformedResponse of string

    type complete = {version : string,
                     status  : int,
		     reason  : string,
                     headers : string Headers.t,
	             body    : string}

    type incomplete = {response        : complete,
	               headersComplete : bool,
                       store           : string}

    type t = complete

    fun new (body: string) : t =
        {
            version = "HTTP/1.1",
            status  = 200,
            reason  = "OK",
            headers = Headers.insert Headers.empty (Header.toKv (Header.ContentLength (String.length body))),
            body    = body
        }

    fun parseFirstLine (line: string, response: Connection.t) : t =
        case String.split (line, " ") of
            [] =>  raise MalformedResponse (line)
          | list => if length (list) <> 3
              then raise MalformedResponse (line)
              else {
                  version = List.nth (list, 0),
                  status  = valOf (Int.fromString (List.nth (list, 1))),
                  reason  = List.nth (list, 2),
                  headers = #headers response,
                  body    = #body response
              }

    fun read (conn) : t =
        let
            val response = Connection.read (conn)
        in
            parseFirstLine (#firstLine response, response)
        end

    fun marshall (response: t) =
        let
            val version = #version response
            val status = Format.int (#status response)
            val reason = #reason response
            val intro = Format.sprintf "% % %\r\n" [version, status, reason]
            val marshalled = map Header.marshall (Headers.toList (#headers response))
            val headers = if length marshalled > 1
                    then foldl (fn (a, b) => String.join ([a, b], "\r\n")) "" marshalled
                else hd marshalled
            val body = #body response
        in
            intro ^ headers ^ "\r\n\r\n" ^ body
        end

    fun write (conn, response: t) : unit =
        Connection.write (conn, marshall response)

    end
end
