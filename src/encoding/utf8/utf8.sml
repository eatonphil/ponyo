structure Utf8 =
struct
    encode (source: string) : byte vector =
        Vector.fromList (String.explode source)

    decode (source: byte vector) : string = ""
end