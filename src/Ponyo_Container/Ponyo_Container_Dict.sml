functor Ponyo_Container_Dict (D: PONYO_TRAIT_HASHABLE) : PONYO_CONTAINER_DICT where type elt = D.t =
struct
    structure T = Ponyo_Container_Map (D);

    type elt = D.t
    type 'a t = ('a T.t) vector * int * int

    val defaultSize = 16
    val loadFactor = 0.75

    fun newWithSize (size: int) : 'a t =
        (Vector.tabulate (size, (fn i => (T.new))), 0, size)

    fun new () : 'a t = newWithSize (defaultSize)

    fun load_ (dict: 'a t) : real =
        let
            val (_, used, size) = dict
        in
            (Real.fromInt used) / (Real.fromInt size)
        end

    fun insert (dict: 'a t) (key: elt) (value: 'a) : 'a t =
        if load_ (dict) > loadFactor then
            let
                val (table, _, size) = dict
                val resized = ref (newWithSize (size * 2))

                fun insertElement (tree) =
                    List.app (fn (key, value) => resized := insert (!resized) key value) (T.toList tree)
            in
                Vector.app insertElement table;
                insert (!resized) key value
            end
        else
            let
                val (table, used, size) = dict
                val length = Word64.fromInt (size)
                val index = Word64.toInt (Word64.mod (D.hash key, length))
                val (current : 'a T.t) = Vector.sub (table, index)
            in
                (Vector.update (table, index, (T.insert current key value)), used + 1, size)
            end

    fun get (dict: 'a t) (key: elt) : 'a =
        let
            val (table, _, size) = dict
            val length = Word64.fromInt (size)
            val index = Word64.toInt (Word64.mod (D.hash key, length))
            val tree = Vector.sub (table, index)
            val value = valOf (T.get tree key)
        in
            value
        end
end