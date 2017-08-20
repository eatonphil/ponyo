functor Ponyo_Container_Dict (D: PONYO_TRAIT_HASHABLE) : PONYO_CONTAINER_DICT where type elt = D.t =
struct
    structure T = Ponyo_Container_Map (D);

    type elt = D.t
    type 'a t = 'a T.t array * int

    val defaultSize = 16
    val loadFactor = 0.75

    fun newWithSize (size: int) : 'a t =
        (Array.tabulate (size, (fn i => T.new)), 0)

    fun new () : 'a t = newWithSize (defaultSize)

    fun load (dict: 'a t) : real =
        let
            val (table, used) = dict
        in
            (Real.fromInt used) / (Real.fromInt (Array.length table))
        end

    fun index ((table, _), key) : int = Word64.toInt (Word64.mod (D.hash key, Word64.fromInt (Array.length table)))

    fun insert (dict: 'a t) (key: elt) (value: 'a) : 'a t =
        if load (dict) > loadFactor then
            let
                val (table, _) = dict
                val resized = ref (newWithSize ((Array.length table) * 2))

                fun insertElement (tree) =
                    List.app (fn (key, value) => resized := insert (!resized) key value) (T.toList tree)
            in
                Array.app insertElement table;
                insert (!resized) key value
            end
        else
            let
                val (table, used) = dict
                val i = index (dict, key)
                val current = Array.sub (table, i)
            in
                Array.update (table, i, T.insert current key value);
                (table, used + 1)
            end

    fun get (dict: 'a t) (key: elt) : 'a option =
        let
            val (table, _) = dict
            val tree = Array.sub (table, (index (dict, key)))
        in
            T.get tree key
        end

    (* This is not going to be particularly efficient. But storing all key
     * values as a list on the structure itself creates a big perf hit. *)
    fun toList ((table, _): 'a t) : (D.t * 'a) list =
        Array.foldr (fn (kvs, allKvs) => (T.toList (kvs) @ allKvs)) [] table
end