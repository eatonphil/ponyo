structure Ponyo_Net_Http_Mime =
struct
    structure String = Ponyo_String
    open String.Dict

    val types =
        let
            val d = String.Dict.new ()
            val d = String.Dict.insert d "js" "application/javascript"
            val d = String.Dict.insert d "css" "text/css"
            val d = String.Dict.insert d "html" "text/html"
            val d = String.Dict.insert d "png" "image/png"
        in
            d
        end
end
