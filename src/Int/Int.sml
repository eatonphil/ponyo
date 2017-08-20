structure Ponyo_Int_internal =
struct
    type t = int

    open Int

    (* http://burtleburtle.net/bob/hash/integer.html *)
    fun hash2 (i: int) : Word64.word =
        let
            val three = Word.fromInt (3)
            val four = Word.fromInt (4)
            val fifteen = Word.fromInt (15)
            val sixteen = Word.fromInt (16)
            val sixtyone = Word32.fromInt (61)
            val magic = Word32.fromInt (0x27d4eb2d)

            val iAsWord = Word32.fromInt (i)

            val a = Word32.xorb (Word32.xorb (iAsWord, sixtyone), Word32.~>> (iAsWord, sixteen))
            val a = Word32.+ (a, Word32.<< (a, three))
            val a = Word32.xorb (a, Word32.~>> (a, four))
            val a = Word32.* (a, magic)
            val a = Word32.xorb (a, Word32.~>> (a, fifteen))
        in
            Word32.toLargeWord (a)
        end

    (* This is currently significantly faster in tests than the above hash2.
     * However, the distribution of keys in the hash table should be worse.
     * Worth more investigation.
     *)
    fun hash (i) = Word64.fromInt (i)
    val unitialized = 0
end
