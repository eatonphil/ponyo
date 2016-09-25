structure Ponyo_Net =
struct
    structure Socket = Ponyo_Net_Socket;
    structure SSLSocket = Ponyo_Net_SSLSocket;
    structure Http = Ponyo_Net_Http;
    structure Https = Ponyo_Net_Https;
end
