structure Ponyo_Container_Option =
struct
    infix 6 >>=
    fun (x: 'a option) >>= (f: 'a -> 'b) : 'b option =
        case x of
            NONE => NONE
          | SOME x => SOME (f x)
end
