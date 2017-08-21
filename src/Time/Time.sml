structure Ponyo_Time :> PONYO_TIME =
struct
    local
        structure Format = Ponyo_Format
        structure Int = Ponyo_Int
        structure String = Ponyo_String

        open CInterface
        val get = get_sym (PONYO_ROOT ^ "/libc.so")
    in

    structure Interval = Ponyo_Time_Interval

    (* Time in nanoseconds, *)
    type t = LargeInt.int

    fun add (time: t) ([]: Interval.t list) : t = time
      | add (time: t) ((hd :: tl): Interval.t list) : t =
        let
            val i = case hd of (* convert to seconds *)
                Interval.Nanosecond n => 1000 * 1000 * 1000 * n
              | Interval.Microsecond m => 1000 * 1000 * m
              | Interval.Millisecond m => 1000 * m
              | Interval.Second s => s
              | Interval.Minute m => m * 60
              | Interval.Hour h => h * 60 * 60
              | Interval.Day d => d * 60 * 60 * 24
        in
            add (time + (LargeInt.fromInt i)) tl
        end

    fun compare (t1: t, t2: t) : order =
        LargeInt.compare (t1, t2)

    fun getNanoseconds (time: t) : t = time

    fun getMicroseconds (time: t) : LargeInt.int =
        (getNanoseconds time) div 1000

    fun getMilliseconds (time: t) : LargeInt.int =
        (getMicroseconds time) div 1000

    fun getSeconds (time: t) : LargeInt.int =
        (getMilliseconds time) div 1000

    fun getMinutes (time: t) : LargeInt.int =
        getSeconds (time) div 60 mod 60

    fun getHour (time: t) : LargeInt.int =
        getSeconds (time) div 3600 mod 24

    fun getDay (time: t) : LargeInt.int =
        getSeconds (time) div 86400 mod 365

    fun getDayOfWeek (time: t) : LargeInt.int =
        getSeconds (time) div 86400 mod 7

    fun getMonth (time: t) : LargeInt.int =
        getSeconds (time) div 86400 div 7 mod 52

    fun getYear (time: t) : LargeInt.int =
        getSeconds (time) div 86400 div 365

    fun now () : t =
        getSeconds (Basis.Time.toNanoseconds (Basis.Time.now ()))

    fun strftime (time: t, format: string, bufferSize: int) : string =
        let
            val buffer = alloc bufferSize (Cpointer Cchar)
            val timeInt = Int.fromLarge (getSeconds time)
            val c_strftime = call4 (get "ponyo_strftime") (LONG, STRING, LONG, POINTER) VOID
        in
            c_strftime (timeInt, format, bufferSize, address buffer);
            fromCstring (buffer)
        end

    fun toISO8601 (time: t) : string =
        strftime (time, "%Y-%m-%dT%H:%M:%SZ", String.length "2011-10-08T07:07:09Z")

    end
end