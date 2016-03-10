signature LIST =
sig
    val contains : 'a list -> 'a -> bool
    val rangeWith : int * int * int -> int list
    val range : int * int -> int list
end

structure List : LIST =
struct
    (* -contains: returns true if the element is within the list.
     *
     *  Ex:
     *      contains ([1, 2], 2) = true
     *      contains ([2, 4], 3) = false
     *)
    fun contains (list: 'a list) (element: 'a) : bool =
        case list of
            [] => false
          | hd :: tl => hd = element orelse contains (tl, element)

    (* -rangeWith: returns a list of integers starting at start and
     *  ending at end exclusively incrementing by increment.
     *
     *  Ex:
     *      rangeWith (0, 4, 1) = [0, 1, 2, 3]
     *      rangeWith (0, 4, 3) = [0, 3]
     *)
    fun rangeWith (start: int, stop: int, increment: int) : int list =
        let
            fun doRange (start: int, stop: int, l: int list) : int list =
                if start >= stop then List.rev l
                else doRange (start + increment, stop, start :: l)
        in
            doRange (start, stop, [])
        end

    (* -range: returns a list of integers starting at start and
     *  ending at end exclusively incrementing by 1.
     *
     *  Ex:
     *      range (0, 4) = [0, 1, 2, 3]
     *      range (2, 5) = [2, 3, 4]
     *)
    and range (start: int, stop: int)
        rangeWith (start, stop, 1)
end
