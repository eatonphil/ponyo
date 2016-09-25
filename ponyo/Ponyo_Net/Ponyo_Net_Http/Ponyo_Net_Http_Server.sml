structure Ponyo_Net_Http_Server =
struct
    local
        structure Format = Ponyo_Format
        structure Socket = Ponyo_Net_Socket

        structure Request  = Ponyo_Net_Http_Request (Socket)
        structure Response = Ponyo_Net_Http_Response (Socket)
    in
    structure Router = Ponyo_Net_Http_Router (Socket)

    val MAX_CONN : int ref = ref ~1

    fun handler (conn, router: Router.t) : unit =
        let
            val request = Request.read (conn);
            val response = router (request);
        in
            Format.println [Request.marshall request];
            Response.write (conn, response);
            Socket.close (conn)
        end

    fun serve (sock, router: Router.t) : unit =
        let
            val (conn, _) = Basis.Socket.accept (sock);
        in
            Thread.Thread.fork (fn () => handler (conn, router), []);
            serve (sock, router);
            ()
        end

    fun bind (sock, address) =
        let
            val sleep = OS.Process.sleep
            fun doBind () = Basis.Socket.bind (sock, address)
        in
            doBind () handle SysError => (sleep (Time.fromSeconds 1); bind (sock, address));
            ()
        end

    fun listenAndServe (address: string, port: int, router: Router.t) : unit =
        let
            val sock = INetSock.TCP.socket ();
        in
            Format.printf "Binding server...\n" [];
            bind (sock, INetSock.any port);
            Format.printf "Server bound. Listening on port %:%\n\n" [address, Int.toString port];
            Basis.Socket.listen (sock, !MAX_CONN);
            Basis.Socket.Ctl.setREUSEADDR (sock, true);
            serve (sock, router);
            Socket.close (sock);
            ()
        end

    end
end
