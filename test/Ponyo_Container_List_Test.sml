structure Ponyo_Container_List_Test =
struct

local
    structure IntList = Ponyo.Container.List(Int)
    val test = Ponyo.Test.test
in

fun run () = [
  (*
    test "List.sort" let
        val c = List.sort
    in [
        c "foobar" = "Foobar",
        c "fOOBAR" = "Foobar",
        c "foobar foobar" = "Foobar foobar"
    ] end
    *)
]

end
end
