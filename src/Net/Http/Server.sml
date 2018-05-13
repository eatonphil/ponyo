structure Ponyo_Net_Http_Server : PONYO_NET_HTTP_SERVER =
struct
    local
        structure Format = Ponyo_Format
        structure Socket = Ponyo_Net_Socket
        structure String = Ponyo_String
        structure Thread = Ponyo_Thread

        structure Request  = Ponyo_Net_Http_Request (Socket)
        structure Response = Ponyo_Net_Http_Response (Socket)
    in
    structure Router = Ponyo_Net_Http_Router (Socket)

    val MAX_CONN : int ref = ref ~1

    fun handleCleanup (conn) : unit =
        let
            val exit = Thread.exit
        in
            Socket.close (conn) handle _ => exit ();
            exit ()
        end

    fun handleRequest (conn) (router: Router.t) : unit =
        let
            val emptyRequest = Request.init (String.Dict.new ()) ""
            val request = Request.read (conn) handle _ => emptyRequest
            val response = router (request) handle _ => Response.InternalServerError;
        in
            Format.println [Request.toString request];
            Response.write conn response handle _ => handleCleanup (conn);
            handleCleanup (conn)
        end

    fun serve sock (router: Router.t) : unit =
        let
            val (conn, _) = Basis.Socket.accept (sock);
        in
            Thread.fork (fn () => handleRequest conn router);
            Thread.run ();
            serve sock router;
            ()
        end

    fun bind sock address =
        let
            val sleep = OS.Process.sleep
            fun doBind () = Basis.Socket.bind (sock, address)
        in
            doBind () handle SysError => (sleep (Time.fromSeconds 1); bind sock address);
            ()
        end

    fun listenAndServe (address: string) (port: int) (router: Router.t) : unit =
        let
            val sock = INetSock.TCP.socket ();
        in
            Format.println ["Binding server..."];
            bind sock (INetSock.any port);
            Format.printf "Server bound. Listening on port %:%\n\n" [address, Int.toString port];
            Basis.Socket.listen (sock, !MAX_CONN);
            Basis.Socket.Ctl.setREUSEADDR (sock, true);
            serve sock router;
            Socket.close (sock);
            ()
        end

    end
end
