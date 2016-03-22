(* Perhaps there's a better way to include these.
 * In any case, their names don't matter.
 *)

functor A (
 D : PONYO_CONTAINER_TREE_BINARYSEARCH_DOMAIN
) : PONYO_CONTAINER_TREE_BINARYSEARCH =
    Ponyo_Container_Tree_BinarySearch (D);

(* Here to be discoverd by Container/ml_bind.ML *)
structure Tree = struct end
