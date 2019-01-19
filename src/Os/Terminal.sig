signature PONYO_OS_TERMINAL =
sig
    datatype color =
        Black
      | Red
      | Green
      | Yellow
      | Blue
      | Magenta
      | Cyan
      | White
      | Reset

    val bold : string -> string
    val code : color -> string
    val colorize : string -> color -> string
end