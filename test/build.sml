PolyML.use "test/Ponyo_String_Test";
PolyML.use "test/Ponyo_Encoding_Json_Test";
PolyML.use "test/Ponyo_Encoding_Base64_Test";

local
    structure Format = Ponyo.Format;
    val test = Ponyo.Test.test;
in

fun main () =
    let
        val _ = Format.println ["\r\n\r\nTesting Ponyo\r\n\r\n"];
        val tests = test "Ponyo" [
            test "Ponyo_String_Test" (Ponyo_String_Test.run ()),
            test "Ponyo_Encoding_Json_Test" (Ponyo_Encoding_Json_Test.run ()),
            test "Ponyo_Encoding_Base64_Test" (Ponyo_Encoding_Base64_Test.run ())       
        ]
    in
        if tests
            then Format.println ["All tests passed!"]
        else (Format.println ["Tests failed."]; OS.Process.exit (OS.Process.failure); ())
    end

end
