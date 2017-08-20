structure T = Ponyo_Char_internal

structure Ponyo_Char =
struct
    open T
    structure Map = Ponyo_Container_Map (T)
    structure List = Ponyo_Container_List (T)
    structure Dict = Ponyo_Container_Dict (T)
end
