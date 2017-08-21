signature PONYO_TIME_INTERVAL =
sig
    datatype t =
        Nanosecond of int
      | Microsecond of int
      | Millisecond of int
      | Second of int
      | Minute of int
      | Hour of int
      | Day of int
end