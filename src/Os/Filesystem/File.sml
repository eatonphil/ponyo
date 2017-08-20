structure Ponyo_Os_Filesystem_File : PONYO_OS_FILESYSTEM_FILE =
struct
    exception NotReader
    exception NotWriter

    datatype mode = Read | Write | Append

    type instream = TextIO.instream
    type outstream = TextIO.outstream

    datatype t = Writer of outstream | Reader of instream

    (* -use: Opens a file for use in one of three modes: Read, Write,
     *  and Append.
     *)
    fun use (path: string, mode: mode) : t =
        case mode of
            Read => Reader (TextIO.openIn path)
	  | Write => Writer (TextIO.openOut path)
          | Append => Writer (TextIO.openAppend path)

    (* -close: Closes an open file. *)
    fun close (Writer file) : unit = TextIO.closeOut (file: TextIO.outstream)
      | close (Reader file) : unit = TextIO.closeIn (file: instream)

    (* -read: Reads all lines of a file and returns a list of the lines. *)
    fun read (Reader file) : string list =
        let
            fun doRead (f: instream, lines: string list) : string list =
                case TextIO.inputLine (f) of
                    NONE => List.rev (lines)
                  | SOME line => doRead (f, line :: lines)
	in
	    doRead (file, [])
	end
      | read (Writer file) = raise NotReader

    (* -readFrom: Opens the file at the path and returns all lines
     *  from the file.
     *)
    fun readFrom (path: string) : string list =
        let
	    val file = use (path, Read)
	    val lines = read (file)
	in
	    close (file);
	    lines
	end

    (* -write: Writes output to the file. *)
    fun write (Writer file, output: string) : unit =
        let in
            TextIO.output (file, output)
        end
      | write (Reader file, _) = raise NotWriter

    (* -writeTo: Opens the file at the path and writes the output
     *  to the file.
     *)
    fun writeTo (path: string, output: string) : unit =
        let
	    val file = use (path, Write)
	in
	    write (file, output);
	    close (file)
	end
end
