functor Ponyo_Container_Tree_BinarySearch (
    D : PONYO_TRAIT_COMPARABLE
) : PONYO_CONTAINER_TREE_BINARYSEARCH where type elt = D.t =
struct
    type elt = D.t

    datatype 'a t = Leaf | Node of 'a t * elt * 'a * 'a t

    (* -new: Returns a new empty tree. *)
    val new : 'a t = Leaf

    (* -insert: Creates a new tree from the original with the given
     *  key-val pair.
     *)
    fun insert (bst: 'a t) (newKey: elt, newVal: 'a) : 'a t =
    	case bst of
	    Leaf => Node (Leaf, newKey, newVal, Leaf)
	  | Node (l, k, v, r) =>
	      let in
		  case D.compare (k, newKey) of
		      LESS => Node (insert l (newKey, newVal), k, v, r)
		    | EQUAL => Node (l, k, v, insert r (newKey, newVal))
	            | GREATER => Node (l, k, v, insert r (newKey, newVal))
	      end

    (* -get: Searches the tree for the key and returns the value if it
     *  exists.
     *)
    fun get (bst: 'a t) (theKey: elt) : 'a option =
        case bst of
	    Leaf => NONE
	  | Node (l, k, v, r) =>
	      let in
	          case D.compare (k, theKey) of
		      LESS => get l theKey
		    | EQUAL => SOME v
		    | GREATER => get r theKey
              end

    fun fromList (list: (elt * 'a) list) : 'a t =
        let
            fun doFromList (list, tree) =
                case list of
                    [] => tree
                  | hd :: tl => doFromList (tl, insert tree hd)
        in
            doFromList (list, new)
        end

    (* -toList: Runs the search tree from left to right and returns the
     *  tree as an ordered list.
     *)
    fun toList (bst: 'a t) : (elt * 'a) list =
        case bst of
	    Leaf => []
	  | Node (l, k, v, r) =>
	      toList (l) @ [(k, v)] @ toList (r)
end
