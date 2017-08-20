structure T = Ponyo_Int_internal

structure Ponyo_Int =
struct
    open T
    structure Map = Ponyo_Container_Map (T)
    structure List = Ponyo_Container_List (T)
    structure Dict = Ponyo_Container_Dict (T)
end
