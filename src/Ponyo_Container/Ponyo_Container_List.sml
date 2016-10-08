functor Ponyo_Container_List (
    D: PONYO_CONTAINER_LIST_DOMAIN
) : PONYO_CONTAINER_LIST where type element = D.t =
struct
    type element = D.t

    local val compare = D.compare in

    open Basis.List

    fun get (list: element list, index: int) : element option =
        if index < 0 then get (list, length list + index)
        else SOME (nth (list, index)) handle _ => NONE

    fun sublistToEnd (list: element list, start: int) : element list =
        take (list, start)

    (* TODO: Should support negative stop.*)
    fun sublist (list: element list, start: int, stop: int) : element list =
        drop (take (list, start), stop - start)

    fun count (list: element list, element: element) : int =
        foldl (fn (el, c) => c + (if el = element then 1 else 0)) 0 list

    fun contains (list: element list, element: element) : bool =
        count (list, element) > 0

    (* TODO: implement stub *)
    fun sort (list: element list) : element list =
        list

    end
end
