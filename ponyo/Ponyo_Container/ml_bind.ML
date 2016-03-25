structure Ponyo_Container =
struct
    structure Tree = Ponyo_Container_Tree
    structure Option = Ponyo_Container_Option
end;

signature PONYO_CONTAINER_TREE_BINARYSEARCH = PONYO_CONTAINER_TREE_BINARYSEARCH
functor A (
    D : PONYO_CONTAINER_TREE_BINARYSEARCH_DOMAIN
) : PONYO_CONTAINER_TREE_BINARYSEARCH =
    Ponyo_Container_Map (D);

signature PONYO_CONTAINER_LIST = PONYO_CONTAINER_LIST;
functor B (
    D : PONYO_CONTAINER_LIST_DOMAIN
) : PONYO_CONTAINER_LIST =
    Ponyo_Container_List (D);
