structure Ponyo_Net_Http =
struct
    structure Method   = Ponyo_Net_Http_Method
    structure Header   = Ponyo_Net_Http_Header
    structure Request  = Ponyo_Net_Http_Request (Ponyo_Net_Socket)
    structure Mime     = Ponyo_Net_Http_Mime
    structure Response = Ponyo_Net_Http_Response (Ponyo_Net_Socket)
    structure Client   = Ponyo_Net_Http_Client (structure Port = struct val port = 80 end
                                                structure Socket = Ponyo_Net_Socket)
    structure Router   = Ponyo_Net_Http_Router (Ponyo_Net_Socket)
    structure Server   = Ponyo_Net_Http_Server
end
