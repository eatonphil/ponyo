structure FormatExport =
struct
    local structure String = StringExport in

    type t = {str: string -> string}

    fun make (fmt: string -> string) : t =
        {str=fmt}

    fun int (i: int) : t =
        make (fn (fmt) => Int.toString (i))

    fun str (s: string) : t =
        make (fn (fmt) => s)

    fun sprintln record accessors =
        case accessors of
	    [] => "\n"
	  | hd :: tl => (#str ((hd record): t) ("")) ^ (sprintln record tl)

    fun println record accessors =
        print (sprintln record accessors)
    end
end
