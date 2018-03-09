signature PONYO_TIME =
sig
    type t

    structure Interval : PONYO_TIME_INTERVAL

    val add : t -> Interval.t list -> t

    val compare : t * t -> order

    val getNanoseconds : t -> LargeInt.int

    val getMicroseconds : t -> LargeInt.int

    val getMilliseconds : t -> LargeInt.int

    val getSeconds : t -> LargeInt.int

    val getMinutes : t -> LargeInt.int

    val getHour : t -> LargeInt.int

    val getDay : t -> LargeInt.int

    val getDayOfWeek : t -> LargeInt.int

    val getMonth : t -> LargeInt.int

    val getYear : t -> LargeInt.int

    val now : unit -> t

    val format : t * string * int -> string

    val toISO8601 : t -> string
end