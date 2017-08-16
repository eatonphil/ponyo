functor Ponyo_Container_Dict (D: PONYO_TRAIT_HASHABLE) : PONYO_CONTAINER_DICT where type elt = D.t =
struct
    structure T = Ponyo_Container_Map (D);

    type elt = D.t
    type 'a t = ('a T.t * bool) vector * int

    val defaultSize = 16
    val loadFactor = 0.75

    fun newWithSize (size: int) : 'a t =
        (Vector.tabulate (size, (fn i => (T.new, false))), 0)

    fun new () : 'a t = newWithSize (defaultSize)

    fun load_ (dict: 'a t) : real =
        let
            val (table, used) = dict
        in
            (Real.fromInt used) / (Real.fromInt (Vector.length table))
        end

    fun insert (dict: 'a t) (key: elt) (value: 'a) : 'a t =
        if load_ (dict) > loadFactor then
            let
                val (table, _) = dict
                val resized = ref (newWithSize (Vector.length (table) * 2))

                fun insertElement (tree, _) =
                    List.app (fn (key, value) => resized := insert (!resized) key value) (T.toList tree)
            in
                Vector.app insertElement table;
                insert (!resized) key value
            end
        else
            let
                val (table, used) = dict
                val length = Word64.fromInt (Vector.length table)
                val index = Word64.toInt (Word64.mod (D.hash key, length))
                val (current : 'a T.t, empty) = Vector.sub (table, index)
                val used = used + (if empty then 1 else 0)
            in
                (Vector.update (table, index, (T.insert current key value, true)), used)
            end

    fun get (dict: 'a t) (key: elt) : 'a =
        let
            val (table, _) = dict
            val length = Word64.fromInt (Vector.length table)
            val index = Word64.toInt (Word64.mod (D.hash key, length))
            val (tree, _) = Vector.sub (table, index)
            val value = valOf (T.get tree key)
        in
            value
        end
end