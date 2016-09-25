structure Ponyo_Encoding_Json_Test =
struct

local
    structure Json = Ponyo.Encoding.Json
    val test = Ponyo.Test.test
in

infix == !=

fun run () = [
    test "Json.parse" let
        val p = Json.parse
        fun a == b = Json.equals (a, b)
        fun a != b = not (Json.equals (a, b))
    in [
        p "{'foo': 1}" == Json.Object [("foo", Json.Int 1)],
        p "{'foo': 2}" != Json.Object [("foo", Json.Int 1)],
        p "{ 'foo' : 'bar' , 't' : 1 }" == Json.Object [("foo", Json.String "bar"), ("t", Json.Int 1)],
        p "{'bar': [1]}" == Json.Object [("bar", Json.List [Json.Int 1])],
        p "{'foo': ['bar', '3', 1, true]}" == Json.Object [("foo", Json.List [Json.String "bar", Json.String "3", Json.Int 1, Json.True])],
        p "{'foo': {'f': 1, 'g': false}}" == Json.Object [("foo", Json.Object [("f", Json.Int 1), ("g", Json.False)])],
        p "{'f':'','g':'1.1.1.1'}" == Json.Object [("f", Json.String ""), ("g", Json.String "1.1.1.1")]
    ] end
]

end
end
