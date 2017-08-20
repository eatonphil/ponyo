structure Ponyo_Format : PONYO_FORMAT =
struct
    local structure String = Ponyo_String in

    val formatVariable : string ref = ref "%"

    (* -sprintf: Returns a formatted string based on the given format
     *  string and the arguments replacing the format variable.
     *
     *  Ex:
     *      sprintf "%: %" ["12/2/24", "ERROR"] = "12/2/24: ERROR"
     *      sprintf "% + % = %" [int 1, int 2, int (1 + 2)] = "1 + 2 = 3"
     *)
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

    (* -printf: Prints the formatted string to stdout. *)
    fun printf (fmt: string) (args: string list) : unit =
	print (sprintf fmt args)

    (* -sprintln: Returns the default formatted string followed by a newline.
     *
     *  Ex:
     *      sprintln ["1", int 1] = "1 1\n"
     *)
    fun sprintln (args: string list) : string =
	let
	    val fmtVar = !formatVariable
	    val len = length args
	    fun mkFmt (i: int, fmt: string) =
		case i of
		    0 => fmt
		| i => mkFmt (i - 1, fmt ^ (if i < len then " " else "") ^ fmtVar)

	    val fmt = (if len = 0 then "" else mkFmt (len - 1, fmtVar)) ^ "\n"
	in
	    sprintf fmt args
	end

    (* -println Prints the formatted string to stdout with a newline and a
     *  flush.
     *)
    fun println (args: string list) : unit =
	let in
	    print (sprintln args);
	    TextIO.flushOut (TextIO.stdOut)
	end

    end
end
