structure Ponyo_Net_Https =
struct
    structure Client   = Ponyo_Net_Https_Client
    structure Response = Ponyo_Net_Http_Response (Ponyo_Net_SSLSocket)
    structure Request  = Ponyo_Net_Http_Request (Ponyo_Net_SSLSocket)
end
