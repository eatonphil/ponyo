structure Ponyo_Net_Http =
struct
    structure Method   = Ponyo_Net_Http_Method
    structure Header   = Ponyo_Net_Http_Header
    structure Headers  = Ponyo_Net_Http_Headers
    structure Client   = Ponyo_Net_Http_Client
    structure Request  = Ponyo_Net_Http_Request (Ponyo_Net_Socket)
    structure Response = Ponyo_Net_Http_Response (Ponyo_Net_Socket)
    structure Router   = Ponyo_Net_Http_Router (Ponyo_Net_Socket)
    structure Server   = Ponyo_Net_Http_Server
end
