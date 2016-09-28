functor Ponyo_Container_Tree_RedBlack (
    D: PONYO_TRAIT_COMPARABLE
) : PONYO_CONTAINER_TREE_REDBLACK where type elt = D.t =
struct
    type elt = D.t
    datatype color = Red | Black
    datatype 'a t =
        Node of ('a t * color * elt * 'a * 'a t)
      | Leaf

    val new : 'a t = Leaf

    fun balance (root: 'a t) : 'a t =
        case root of
            Node (Node (Node (l1, Red, k1, v1, r1), Red, k2, v2, r2), Black, k3, v3, r3) =>
              Node (Node (l1, Black, k1, v1, r1), Red, k2, v2, Node (r2, Black, k3, v3, r3))
          | Node (Node (l1, Red, k1, v1, Node (l2, Red, k2, v2, r2)), Black, k3, v3, r3) =>
              Node (Node (l1, Black, k1, v1, l2), Red, k2, v2, Node (r2, Black, k3, v3, r3))
          | Node (l1, Black, k1, v1, Node (Node (l2, Red, k2, v2, r2), Red, k3, v3, r3)) =>
              Node (Node (l1, Black, k1, v1, l2), Red, k2, v2, Node (r2, Black, k3, v3, r3))
          | Node (l1, Black, k1, v1, Node (l2, Red, k2, v2, Node (l3, Red, k3, v3, r3))) =>
              Node (Node (l1, Black, k1, v1, l2), Red, k2, v2, Node (l3, Black, k3, v3, r3))
          | _ => root

    fun blacken (root: 'a t) : 'a t =
        case root of
            Node (l, Red, k, v, r) => Node (l, Black, k, v, r)
          | root => root

    fun insert (root: 'a t) (key: elt) (value: 'a) : 'a t =
        let
            fun insertHelper (t: 'a t) : 'a t =
                case t of
                    Leaf => Node (Leaf, Red, key, value, Leaf)
                  | Node (left, color, childKey, childValue, right) =>
                case D.compare (key, childKey) of
                    LESS => balance (Node (insertHelper left, color, childKey, childValue, right))
                  | EQUAL => Node (left, color, childKey, childValue, right)
                  | GREATER => balance (Node (left, color, childKey, childValue, insertHelper right))
        in
            insertHelper (blacken root)
        end

    fun get (root: 'a t) (key: elt) : 'a option =
        case root of
            Leaf => NONE
          | Node (l, _, k, v, r) =>
        case D.compare (key, k) of
            LESS => get l key
          | EQUAL => SOME v
          | GREATER => get r key

    fun toList (root: 'a t) : (elt * 'a) list =
        (* TODO: use tail recursion *)
        case root of
            Leaf => []
          | Node (l, _, k, v, r) =>
        toList (l) @ ((k, v) :: toList (r))

    fun fromList (l: (elt * 'a) list) : 'a t =
        let
            fun doFromList root l =
                case l of
                    [] => new
                  | (k, v) :: tail =>
                doFromList (insert root k v) l
        in
            doFromList new l
        end
end
