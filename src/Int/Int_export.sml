structure Ponyo_Int_ =
struct
    open Ponyo_Int
    structure Map = Ponyo_Container_Map (Ponyo_Int)
    structure List = Ponyo_Container_List (Ponyo_Int)
    structure Dict = Ponyo_Container_Dict (Ponyo_Int)
end
