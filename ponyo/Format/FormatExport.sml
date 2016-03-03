structure FormatExport =
struct
    local structure String = StringExport in

    val formatVariable : string ref = ref "%"

    fun int (i: int) = Int.toString (i)
    fun real (r: real) = Real.toString (r)
    fun str (s: string) = "\" ^ s ^ \""
    fun char (c: char) = Char.toString (c)
    fun list (l: string list) =
        "[" ^ (String.join (l, ", ")) ^ "]"

    fun sprintf (fmt: string) (args : string list) : string =
	let
	    val formatVariable = !formatVariable

	    fun isEscape (index: int) : bool =
		String.length (fmt) > index + 1 andalso
		String.hasPrefix (String.substringToEnd (fmt, index + 1), formatVariable)

	    fun sprintfNext (index: int, args) : string =
		sprintf (String.substringToEnd (fmt, index)) args

	    fun escapedLength (start: int) : int =
		start + 2 * (String.length formatVariable)

	    fun replaceAndContinue (index: int) : string =
		let
		    val (replacement, args) = case args of
		        [] => (formatVariable, [])
		      | hd :: tl => (hd, tl)

                    val offset = index + String.length (formatVariable)
		in
		    replacement ^ sprintfNext (offset, args)
		end
	in
	    case String.indexOf (fmt, formatVariable) of
	        ~1 => fmt
	       | i => String.substring (fmt, 0, i) ^
	           (if isEscape (i)
		        then formatVariable ^ sprintfNext (escapedLength (i), args)
                    else
		        replaceAndContinue (i))
	end

    fun printf (fmt: string) (args: string list) : unit =
	print (sprintf fmt args)

    fun sprintln (args: string list) : string =
	let
	    val fmtVar = !formatVariable
	    val len = length args
	    fun mkFmt (i: int, fmt: string) =
		case i of
		    0 => fmt
		| i => mkFmt (i - 1, fmt ^ (if i < len then " " else "") ^ fmtVar)

	    val fmt = if len = 0 then "" else mkFmt (len - 1, fmtVar) ^ "\n"
	in
	    sprintf fmt args
	end

    fun println (args: string list) : unit =
	print (sprintln args)

    end
end
