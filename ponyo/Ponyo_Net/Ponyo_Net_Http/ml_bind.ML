structure Ponyo_Net_Http =
struct
    structure Method   = Ponyo_Net_Http_Method
    structure Header   = Ponyo_Net_Http_Header
    structure Headers  = Ponyo_Net_Http_Headers
    structure Request  = Ponyo_Net_Http_Request
    structure Response = Ponyo_Net_Http_Response
    structure Client   = Ponyo_Net_Http_Client
    structure Server   = Ponyo_Net_Http_Server
end
