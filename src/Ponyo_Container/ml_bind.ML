structure Ponyo_Container =
struct
    structure Tree = Ponyo_Container_Tree
    structure Option = Ponyo_Container_Option
end;

signature PONYO_CONTAINER_TREE_BINARYSEARCH = PONYO_CONTAINER_TREE_BINARYSEARCH;
functor A (
    D : PONYO_TRAIT_COMPARABLE
) : PONYO_CONTAINER_TREE_BINARYSEARCH =
    Ponyo_Container_Tree_BinarySearch (D);

signature PONYO_CONTAINER_TREE_REDBLACK = PONYO_CONTAINER_TREE_REDBLACK;
functor B (
    D : PONYO_TRAIT_COMPARABLE
) : PONYO_CONTAINER_TREE_REDBLACK =
    Ponyo_Container_Tree_RedBlack (D);

signature PONYO_CONTAINER_MAP = PONYO_CONTAINER_MAP;
functor C (
    D : PONYO_TRAIT_COMPARABLE
) : PONYO_CONTAINER_MAP =
    Ponyo_Container_Map (D);

signature PONYO_CONTAINER_LIST = PONYO_CONTAINER_LIST;
functor E (
    D : PONYO_CONTAINER_LIST_DOMAIN
) : PONYO_CONTAINER_LIST =
    Ponyo_Container_List (D);

signature PONYO_CONTAINER_DICT = PONYO_CONTAINER_DICT;
functor F (
    D: PONYO_TRAIT_HASHABLE
) : PONYO_CONTAINER_DICT =
    Ponyo_Container_Dict (D);
