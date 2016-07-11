structure Ponyo_Char =
struct
    type t = char
    open Char
end

structure Ponyo_Char_ =
struct
    open Ponyo_Char
    structure Map = Ponyo_Container_Map (Ponyo_Char)
    structure List = Ponyo_Container_List (Ponyo_Char)
end
