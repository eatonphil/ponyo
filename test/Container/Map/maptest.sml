structure Int =
struct
    open Int
    type t = int
end;

structure IntMap = Ponyo_Container_Map (Int);

val BAD_PASSWORD = 1
val BAD_USERNAME = 1

infix 6 |>
fun x |> f = f x

val ERROR_CODES = IntMap.fromList [
    (BAD_PASSWORD, "You have provided a bad password."),
    (BAD_USERNAME, "You have provided a bad username.")
]

fun main () =
    let in
        PolyML.print (valOf (IntMap.get ERROR_CODES BAD_PASSWORD));
        ()
    end
