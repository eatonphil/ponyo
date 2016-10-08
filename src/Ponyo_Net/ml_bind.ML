structure Ponyo_Net =
struct
    structure Socket = Ponyo_Net_Socket;
    structure SSLSocket = Ponyo_Net_SSLSocket;
    structure Http = Ponyo_Net_Http;
    structure Https =
    struct
        structure Client = Ponyo_Net_Http_Client (
            structure Port = struct val port = 443 end
            structure Socket = Ponyo_Net_SSLSocket)
    end
end
