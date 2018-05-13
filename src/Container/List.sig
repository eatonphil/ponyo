(*
 *  PONYO_CONTAINER_LIST: This is the resulting signature of the
 *  Ponyo_Container_List functor.
 *
 *  Ex:
 *      structure IntList = Ponyo_Container_List (Int);
 *      val sortedInts = IntList.sort [2, 1, 3, 4];
 *)
signature PONYO_CONTAINER_LIST =
sig
    type element

    (*
     *  contains: This returns true if the given list contains
     *  the given element at least once.
     *
     *  Ex:
     *      contains ([1, 3, 2, 2], 2) = true
     *)
    val contains : element list * element -> bool

    (*
     *  count: This returns the number of elements equal to the
     *  given element within the given list.
     *
     *  Ex:
     *      count ([1, 2, 3, 2, 2], 2) = 3
     *)
    val count : element list * element -> int
    
    (*
     *  get: This returns the nth item in the list. If the index
     *  is negative, it is added to the length of the list
     *  repeatedly until it is non-negative.
     *
     *  Ex:
     *      get ([1, 3, 2, 2], 0)  = 1
     *      get ([1, 3, 2, 2], ~1) = 2
     *      get ([1, 3, 2, 2], ~7) = 3
     *)
    val get : element list * int -> element option

    (*
     *  sort: This returns a new list with each element in order
     *  from least to greatest according to the compare function
     *  for the elements.
     *
     *  Ex:
     *      sort [2, 1, 4, 3] = [1, 2, 4, 3]
     *)
    val sort : element list -> element list

    (*
     *  sublist: This returns a section of the given list
     *  starting with the first index and counting forward using
     *  the second index.
     *
     *  Ex:
     *      sublist ([1, 2, 3, 4], 1, 3) = [2, 3, 4]
     *)
    val sublist : element list * int * int -> element list

    (*
     *  sublistToEnd: This returns a section of the given list
     *  starting with the first index and ending at the end of
     *  the list.
     *
     *  Ex:
     *      sublistToEnd ([1, 2, 3, 4], 1) = [2, 3, 4]
     *)
    val sublistToEnd : element list * int -> element list

    val join : element list * string -> string

    val toString : element list -> string
end

signature PONYO_CONTAINER_LIST_DOMAIN =
sig
    eqtype t

    val compare : t * t -> order
    val toString : t -> string
end
