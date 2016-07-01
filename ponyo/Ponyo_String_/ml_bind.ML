structure Ponyo_String_ =
struct
    open Ponyo_String
    structure Map = Ponyo_Container_Map (Ponyo_String)
    structure List = Ponyo_Container_List (Ponyo_String)
end
