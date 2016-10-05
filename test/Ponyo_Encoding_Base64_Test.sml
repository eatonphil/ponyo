structure Ponyo_Encoding_Base64_Test =
struct
    local
        structure Base64 = Ponyo.Encoding.Base64
        val test = Ponyo.Test.test
    in

    fun run () = [
        test "Json.parse" let
            val e = Base64.Encode.fromString
        in [
            e "" = "",
            e "f" = "Zg==",
            e "fo" = "Zm8=",
            e "foo" = "Zm9v",
            e "foob" = "Zm9vYg==",
            e "fooba" = "Zm9vYmE=",
            e "foobar" = "Zm9vYmFy",
            e "Dan" = "RGFu",
            e "D" = "RA==",
            e "DANIEL" = "REFOSUVM", (* <- This looks oddly like Latin. *)
            e "Lorem ipsum dolor sit amet, consectetur adipiscing elit." = "TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdC4="
        ] end
    ]

    end
end
