structure Ponyo_Net_Https =
struct
    structure Client = Ponyo_Net_Http_Client (
        structure Port = struct val port = 443 end
        structure Socket = Ponyo_Net_SSLSocket)
end
