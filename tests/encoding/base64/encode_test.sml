structure Base64EncodeTest =
struct
    structure Base64 = Ponyo.Encoding.Base64

    fun main () =
        let
            fun test (input: string, expectedOutput: string) =
                let
                    val actualOutput = Base64.Encode.fromString(input)
                in
                    if expectedOutput = actualOutput
                        then
                            print ("Pass: base64Encode(\"" ^ input ^ "\") = \"" ^ expectedOutput ^ "\".\n")
                        else
                            raise Fail ("base64Encode(\"" ^ input ^ "\") resulted in \"" ^ actualOutput ^
                                    "\" but was expected to be \"" ^ expectedOutput ^ "\".")
                end
        in
            List.app test
            [
                ("", ""),
                ("f", "Zg=="),
                ("fo", "Zm8="),
                ("foo", "Zm9v"),
                ("foob", "Zm9vYg=="),
                ("fooba", "Zm9vYmE="),
                ("foobar", "Zm9vYmFy"),
                ("Dan", "RGFu"),
                ("D", "RA=="),
                ("DANIEL", "REFOSUVM"), (* <- This looks oddly like Latin. *)
                ("Lorem ipsum dolor sit amet, consectetur adipiscing elit.", "TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdC4=")
            ]
        end

    val () = main()
end