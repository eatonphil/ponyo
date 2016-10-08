structure Ponyo_Time_Interval =
struct
    type t =
        Nanosecond of int
      | Microsecond of int
      | Millisecond of int
      | Second of int
      | Minute of int
      | Hour of int
      | Day of int
end

structure Ponyo_Time :> PONYO_TIME =
struct
    structure Interval = Ponyo_Time_Interval

    (* Time is stored in seconds in Poly/ML. Does this mean sub-second timing is not possible? *)
    type t = LargeInt.int

    fun now () : t =
        Basis.Time.toSeconds (Basis.Time.now ())

    fun add (time: t) ([]: Interval.t list) : t = t
      | add (time: t) ((hd :: tl): Interval.t list) : t =
        let
            val i = case hd of (* convert to seconds *)
                Nanosecond n => 1000 * 1000 * 1000 * n
              | Microsecond m => 1000 * 1000 * m
              | Millisecond m => 1000 * m
              | Second s => s
              | Minute m => m * 60
              | Hour h => h * 60 * 60
              | Day d => d * 60 * 60 * 24
        in
            add (t + i) tl
        end

    fun compare (t1: t, t2: t) : order =
        LargeInt.compare (t1, t2)

    fun format (time: t) (fmt: string) : string =
        ""
end
