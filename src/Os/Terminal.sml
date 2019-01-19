structure Ponyo_Os_Terminal : PONYO_OS_TERMINAL =
struct
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

    fun code Black = "\u001b[30m"
      | code Red = "\u001b[31m"
      | code Green = "\u001b[32m"
      | code Yellow = "\u001b[33m"
      | code Blue = "\u001b[34m"
      | code Magenta = "\u001b[35m"
      | code Cyan = "\u001b[36m"
      | code White = "\u001b[37m"
      | code Reset = "\u001b[0m"

    fun colorize (text: string) (color: color) : string =
        code (color) ^ text ^ code (Reset)

    fun bold (text: string) : string =
        "\u001b[37;1m" ^ text ^ code (Reset)
end