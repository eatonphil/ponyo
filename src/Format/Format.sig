signature PONYO_FORMAT =
sig
    (*
     *  sprintf: This returns a new string from the format string and
     *  arguments.
     *
     *  Ex:
     *      sprintf "%: %" ["12/2/24", "ERROR"] = "12/2/24: ERROR"
     *      sprintf "% + % = %" [int 1, int 2, int (1 + 2)] = "1 + 2 = 3"
     *)
    val sprintf : string -> string list -> string

    (*  printf: Prints the formatted string to stdout. *)
    val printf : string -> string list -> unit

    (*
     *  sprintln: This concatenates each argument and adds a newline.
     *
     *  Ex:
     *      sprintln ["1", Int.toString 1] = "1 1\n"
     *)
    val sprintln : string list -> string

    (*
     *  println: Prints the formatted string to stdout with a newline and
     *  flushes stdout.
     *)
    val println : string list -> unit
end
