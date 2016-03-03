signature FILE
struct
    type mode
    type t

    val close : t -> unit
    val open : string * string -> t
    val read : t -> string list
    val readFile : string -> string list
    val write : t * string -> unit
end

structure File : FILE =
struct
    datatype mode = Read | Write | Append

    datatype t = Writer of outstream | Reader of instream

    fun open (path: string, mode: mode) : t =
        case mode of
	    Read => TextIO.openIn (path)
	  | Write => TextIO.openOut (path)
	  | Append => TextIO.openAppend (path)

    fun close (file: t) : unit =
        

    fun read (Writer file) : string list =
        let
	    fun doRead (f: instream, lines: string list) : string list =
	        case TextIO.inputLine (f) of
		    "" => List.rev (lines)
		  | line => doRead (f, line :: lines)
	in
	    doRead (file, [])
	end
      | read (Reader file) : string list =
        raise 

    fun readFile (path: string) : string list =
        let
	    val file = TextIO.openIn (path)
	in
	    read (file);
	    close (file)
	end

    fun write (Writer file, output: string) : unit =
        let
	    val vec : vector = Byte.stringToBytes (output)
	in
            TextIO.output (file, vec)
        end
end
