structure Headers =
struct
    local
        structure String = StringExport
	infix 6 >>=;

	fun a >>= f = case a of
	    NONE => NONE
	  | SOME a => SOME (f a)
    in

    structure HeadersBst = Bst(String);

    open HeadersBst;

    fun get (hs: string HeadersBst.t) (h: string) : Header.t option =
	HeadersBst.get hs h >>= (fn v => Header.fromKv (h, v))

    fun toList (hs: string HeadersBst.t) : Header.t list =
        map Header.fromKv (HeadersBst.toList hs)

    end
end
