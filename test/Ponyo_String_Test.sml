structure Ponyo_String_Test =
struct

local
    structure String = Ponyo.String
    val test = Ponyo.Test.test
in

fun run () = [
    test "String.capitalize" let
        val c = String.capitalize
    in [
        c "foobar" = "Foobar",
        c "fOOBAR" = "Foobar",
        c "foobar foobar" = "Foobar foobar"
    ] end,

    test "String.charAt" let
        val c = String.charAt
    in [
        c ("foobar", 0) = #"f",
        c ("foobar", ~1) = #"r",
        c ("foobar", 6) = #"f" handle String.IndexError _ => true | e => false
    ] end,

    test "String.compare" let
        val c = String.compare
    in [
        c ("foobar", "foobar") = EQUAL,
        c ("foobar", "goobar") = LESS,
        c ("foobar", "eoobar") = GREATER
    ] end,

    test "String.count" let
        val c = String.count
    in [
        c ("foobar", "foobar") = 1,
        c ("foobar", "foo") = 1,
        c ("foobar", "o") = 2,
        c ("foobar", "foocar") = 0,
        c ("", "foobar") = 0,
        c ("foobar", "") = 0
    ] end,

    test "String.explode" let
        val e = String.explode
    in [
        e ("foobar") = [#"f", #"o", #"o", #"b", #"a", #"r"],
        e ("") = []
    ] end,

    test "String.fromChar" let
        val f = String.fromChar
    in [
        f (#"c") = "c"
    ] end,

    test "String.hasPrefix" let
        val h = String.hasPrefix
    in [
        h ("foobar", "foo"),
        h ("foobar", ""),
        not (h ("foobar", "bar")),
        not (h ("", "foobar"))
    ] end,

    test "String.hasSubstring" let
        val h = String.hasSubstring
    in [
        h ("foobar", "foo"),
        h ("foobar", "bar"),
        h ("foobar", "oba"),
        h ("foobar", "foobar"),
        not (h ("foobar", "")),
        not (h ("foobar", "barf")),
        not (h ("foobar", "ofoo")),
        not (h ("", "foobar"))
    ] end,

    test "String.hasSuffix" let
        val h = String.hasSuffix
    in [
        h ("foobar", "bar"),
        h ("foobar", ""),
        not (h ("foobar", "foo")),
        not (h ("", "foobar"))
    ] end,

    test "String.implode" let
        val i = String.implode
    in [
        i ([#"f", #"o", #"o"]) = "foo",
        i ([]) = ""
    ] end,

    test "String.indexOfFrom" let
        val i = String.indexOfFrom
    in [
        i ("foobar", "o", 0) = 1,
        i ("foobar", "o", 2) = 2,
        i ("foobar", "f", 1) = ~1
    ] end,

    test "String.indexOf" let
        val i = String.indexOf
    in [
        i ("foobar", "o") = 1,
        i ("foobar", "g") = ~1
    ] end,

    test "String.isAlphaNum" let
        val i = String.isAlphaNum
    in
        [i ("abcdefghijklmnopqrstuvwxyz0123456789")] @
        map (fn c => not (i (String.fromChar c))) (String.explode ".,<>/?;'\":[]\\{}|=-+_)(*&^%$#@!")
    end,

    test "String.isChar" let
        val i = String.isChar
    in [
        i ("f"),
        not (i ("foo"))
    ] end,

    test "String.isDigit" let
        val i = String.isDigit
    in [
        i ("0123456789"),
        not (i "abcdefghijklmnopqrstuvwxyz")
    ] end,

    test "String.isLower" let
        val i = String.isLower
    in [
        i ("foobar"),
        not (i "Foobar"),
        not (i "FOOBAR")
    ] end,

    test "String.isUpper" let
        val i = String.isUpper
    in [
        i ("FOOBAR"),
        not (i "Foobar"),
        not (i "foobar")
    ] end,

    test "String.join" let
        val j = String.join
    in [
        j (["foo", "bar"], "") = "foobar",
        j (["foo", "bar"], "kit") = "fookitbar",
        j (["foo", ""], "") = "foo",
        j ([], "") = "",
        j ([], "kit") = ""
    ] end,

    test "String.length" let
        val l = String.length
    in [
        l ("foobar") = 6,
        l ("") = 0
    ] end,

    test "String.map" let
        val m = String.map
    in [
        m "foobar" Char.toUpper = "FOOBAR"
    ] end,

    test "String.replace" let
        val r = String.replace
    in [
        r ("foobar", "oba", "obarfooba") = "foobarfoobar",
        r ("foobarfoobar", "foobar", "") = "",
        r ("foobarfoobar", "oba", "abo") = "foaborfoabor",
        r ("foobar", "roof", "foor") = "foobar",
        r ("foobar", "", "foo") = "foobar"
    ] end,

    test "String.reverse" let
        val r = String.reverse
    in [
        r ("foobar") = "raboof",
        r ("") = ""
    ] end,

    test "String.splitN" let
        val s = String.splitN
    in [
        s ("foobarfoobarfoobar", "foobar", 1) = ["", "foobarfoobar"],
        s ("foobar", "o", 1) = ["f", "obar"],
        s ("www.google.com/", "/", 1) = ["www.google.com", ""]
    ] end
]

end
end
