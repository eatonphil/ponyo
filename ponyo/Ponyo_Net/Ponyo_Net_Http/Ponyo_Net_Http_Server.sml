structure Ponyo_Net_Http_Server =
struct
    local
        structure Format = Ponyo_Format

        structure Request  = Ponyo_Net_Http_Request
        structure Response = Ponyo_Net_Http_Response
    in

    type router = Request.t -> Response.t

    val MAX_CONN : int ref = ref ~1

    fun handler (conn, router: router) : unit =
        let
            val request = Request.read (conn);
            val response = router (request);
        in
            Format.println [Request.marshall request];
            Response.write (conn, response);
            Socket.close (conn)
        end

    fun serve (sock, router: router) : unit =
        let
            val accept = Socket.acceptNB (sock);
            fun fork (conn) =
                Thread.Thread.fork (fn () => handler (conn, router), [])
        in
            case accept of
                NONE => ()
              | SOME (conn, _) => (fork conn; ());
            serve (sock, router);
            ()
        end

    fun bind (sock, address) =
        let
            val sleep = OS.Process.sleep
            fun doBind () = Socket.bind (sock, address)
        in
            doBind () handle SysError => (sleep (Time.fromSeconds 1); bind (sock, address));
            ()
        end

    fun listenAndServe (address: string, port: int, router: router) : unit =
        let
            val sock = INetSock.TCP.socket ();
        in
            Format.printf "Binding server...\n" [];
            bind (sock, INetSock.any port);
            Format.printf "Server bound. Listening on port %:%\n\n" [address, Int.toString port];
            Socket.listen (sock, !MAX_CONN);
            Socket.Ctl.setREUSEADDR (sock, true);
            serve (sock, router);
            Socket.close (sock);
            ()
        end

    end
end
