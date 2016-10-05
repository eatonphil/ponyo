structure Ponyo_Encoding_Base64 =
struct
    local
        structure String = Basis.String
        structure Word8Vector = Word8Vector

        val base64Digits =
            let
                val upperAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                val lowerAlphabet = String.map Char.toLower upperAlphabet
                val decimalDigits = "0123456789"
                val misc = "+/="
            in
                upperAlphabet ^ lowerAlphabet ^ decimalDigits ^ misc
            end

        val op&&& = Word8.andb
        val op||| = Word8.orb
        val op<< = Word8.<<
        val op>> = Word8.~>>
        val invert = Word8.notb

        infix 6 <<
        infix 6 >>
        infix 5 &&&
        infix 4 |||

        (**
         * Here's a brief explanation of Base64 encoding.
         *  Spec: https://tools.ietf.org/html/rfc4648
         *  Wikipedia: https://en.wikipedia.org/wiki/Base64
         *
         * The idea is that you first start out with three octets (i.e. bytes for most people).
         * You then split these bytes into four 6-bit entities (or "sextets").
         * Each of these 6-bit entities maps to at most 63 (6 bits means 2^6 - 1),
         * and can be used to index into a digit string (defined above).
         *
         * As an exaple, imagine we have the ASCII string "Dan".
         * You can visualize it like so:
         *
         * Labels     |        a       |       b       |       c       |
         * -----------+----------------+---------------+---------------+
         * ASCII      |        D       |       a       |       n       |
         * As octets  |       68       |      97       |     110       |
         *            |                |               |               |
         * Bits       | 0 1 0 0 0 1 0 0 0 1 1 0 0 0 0 1 0 1 1 0 1 1 1 0|
         *            |            |           |           |           |
         * As sextets |     17     |     6     |     5     |     46    |
         * Base-64    |      R     |     G     |     F     |      u    |
         * -----------+------------+-----------+-----------+-----------+
         * Labels     |      w     |     x     |     y     |      z    |
         *
         * We'll call the input octets 'a', 'b', and 'c', and the output sextets 'w', 'x', 'y', and 'z'.
         * Here, (a = 'D', b = 'a', c = 'n'), and (w = 'R', x = 'G', y = 'F', z = 'u').
         * You can repeat this process over and over again to encode arbitrarily long binary data into
         * an ASCII representation.
         *
         * Now in some cases, we won't have three full octets.
         * For instance, if we're encoding a single byte, we'll end up with
         * something like the following:
         *
         * Labels     |        a       |       b       |       c       |
         * -----------+----------------+---------------+---------------+
         * ASCII      |        D       |      N/A      |      N/A      |
         * As octets  |       68       |      N/A      |      N/A      |
         *            |                |               |               |
         * Bits       | 0 1 0 0 0 1 0 0 - - - - - - - - - - - - - - - -|
         *            |            |           |           |           |
         * As sextets |     17     |     ?     |     ?     |     ?     |
         * Base-64    |      R     |     ?     |     ?     |     ?     |
         * -----------+------------+-----------+-----------+-----------+
         * Labels     |      w     |     x     |     y     |      z    |
         *
         * We could use the same scheme as before with 'b' & 'c' set to 0 and
         * just set our outputs to "RAAA"; however, if we did that and tried to rebuild our inputs,
         * the octet value of 'b' and 'c' would both be 0 (which is NUL).
         * The problem is we need to encode our length.
         * We could just omit 'y' and 'z' to signal a the lack of a 'b' and 'c' respectively,
         * but RFC 4648 instead calls for using a padding character.
         * In this implementation, the padding character is '=' which is accessible at index 64.
         *
         * If both 'b' and 'c' are missing, then 'y' and 'z' are represented by
         * padding characters.
         *
         * If *just* 'c' is missing, then only 'z' is assigned that sentinel value.
         * So in the above, case, we end up with "RA==".
         *
         * When decoding from base64, '=' just has the value 0 when represented as
         * the appropriate sextet of bits so that they can still be read from.
         *)

        (* A structure of bit masks to we'll use. *)
        structure Masks =
        struct
            val LowerCToZ           = (* 00111111 *) 0wx3F
            val UpperCToLowerY      = (* 11000000 *) invert LowerCToZ
            val LowerBToUpperY      = (* 00001111 *) 0wxF
            val UpperBToLowerX      = (* 11110000 *) invert LowerBToUpperY
            val LowerAToUpperX      = (* 00000011 *) 0wx3
            val UpperAToPreshiftedW = (* 11111100 *) invert LowerAToUpperX
        end

        fun getW (a: Word8.word) =  (a &&& Masks.UpperAToPreshiftedW) >> 0w2

        fun getX (a: Word8.word, b: Word8.word) =
            let
                val upperX = (a &&& Masks.LowerAToUpperX) << 0w4
                val lowerX = (b &&& Masks.UpperBToLowerX) >> 0w4
            in
                upperX ||| lowerX
            end

        fun getY (b: Word8.word, c: Word8.word) =
            let
                val upperY = (b &&& Masks.LowerBToUpperY) << 0w2
                val lowerY = (c &&& Masks.UpperCToLowerY) >> 0w6
            in
                upperY ||| lowerY
            end

        fun getZ (c: Word8.word) = (c &&& Masks.LowerCToZ)

        (* Get a Base64 digit from one of the words produced from encoding. *)
        fun getBase64Digit (word: Word8.word) = String.sub(base64Digits, Word8.toInt word)

        (* Get a Base64 string given a list of words produced from encoding. *)
        fun getBase64String (ws: Word8.word list) = String.implode (List.map getBase64Digit ws)

        fun fromOctetTriplet (a: Word8.word, b: Word8.word, c: Word8.word) =
            getBase64String [getW(a), getX(a, b), getY(b, c), getZ(c)]

        fun fromOctetPair (a: Word8.word, b: Word8.word) =
            getBase64String [getW(a), getX(a, b), getY(b, 0w0), 0w64]

        fun fromOctetSingleton (a: Word8.word) =
            getBase64String [getW(a), getX(a, 0w0), 0w64, 0w64]

        fun base64Encode (i: int, data: Word8Vector.vector, dataLength: int, accumulator: string) =
            if i >= dataLength
            then
                accumulator
            else
                let
                    val next = case dataLength - i of
                        1 => fromOctetSingleton(Word8Vector.sub(data, i))
                      | 2 => fromOctetPair(Word8Vector.sub(data, i), Word8Vector.sub(data, i + 1))
                      | _ => fromOctetTriplet (Word8Vector.sub(data, i), Word8Vector.sub(data, i + 1), Word8Vector.sub(data, i + 2))
                in
                    base64Encode (i + 3, data, dataLength, accumulator ^ next)
                end
    in
        structure Encode =
        struct
            fun fromWord8Vector (data: Word8Vector.vector) = base64Encode(0, data, Word8Vector.length data, "")

            val fromWord8List = fromWord8Vector o Word8Vector.fromList

            val fromString = fromWord8List o
                                (List.map (Word8.fromInt o Char.ord)) o
                                    String.explode;
        end
    end

end