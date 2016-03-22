structure Container =
struct
    structure Tree = Tree
    structure Option = Ponyo_Option
end

(*
 * This is initially redefined in Container/Ponyo_Container_Map
 * to avoid forcing reliance on Poly/ML compilation scheme.
 *)
functor A (
 D : PONYO_CONTAINER_TREE_BINARYSEARCH_DOMAIN
) : PONYO_CONTAINER_TREE_BINARYSEARCH =
    Ponyo_Container_Map (D);

