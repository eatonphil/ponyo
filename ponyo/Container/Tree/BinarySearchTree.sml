signature BINARYSEARCHTREE =
sig
    type elt
    type 'a t

    val empty : 'a t
    val insert : 'a t -> (elt * 'a) -> 'a t
    val get : 'a t -> elt -> 'a option
    val toList : 'a t -> (elt * 'a) list
end

signature Ord =
sig
    type t

    val compare : t * t -> order
end

structure BinarySearchTree = struct end;

functor Ponyo_Container_BinarySearchTree (O: Ord) : BINARYSEARCHTREE where type elt = O.t =
struct
    type elt = O.t

    datatype 'a t = Leaf | Node of 'a t * elt * 'a * 'a t

    (* -empty: Returns a new empty tree. *)
    val empty : 'a t = Leaf

    (* -insert: Creates a new tree from the original with the given
     *  key-val pair.
     *)
    fun insert (bst: 'a t) (newKey: elt, newVal: 'a) : 'a t =
    	case bst of
	    Leaf => Node (Leaf, newKey, newVal, Leaf)
	  | Node (l, k, v, r) =>
	      let in
		  case O.compare (k, newKey) of
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
	          case O.compare (k, theKey) of
		      LESS => get l theKey
		    | EQUAL => SOME v
		    | GREATER => get r theKey
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
