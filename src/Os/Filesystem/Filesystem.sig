(*
 *  PONYO_OS_FILESYSTEM: this is a set of tools for interacting with a filesystem.
 *  All paths sent to functions in this module are expanded. See the `expand` for
 *  for more information.
 *)
signature PONYO_OS_FILESYSTEM =
sig
    structure File : PONYO_OS_FILESYSTEM_FILE

    (*  exists: returns true if the path given is a valid file, directory,
     *  or symbolic link.
     *)
    val exists : string -> bool

    (*  expand: expands paths beginning with a tilde if $HOME is set. *)
    val expand : string -> string

    (*
     *  makeDirectory: creates a new directory at the specified path.
     *  If one already exists, this does not fail.
     *)
    val makeDirectory : string -> unit

    (*
     *  remove: deletes a file at the specified path. Use `removeDirectory`
     *  to remove a directory.
     *
     *  Returns true on success.
     *)
    val remove : string -> bool

    (*
     *  removeDirectory: deletes a directory at the specified path.
     *
     *  Returns true on success.
     *)
    val removeDirectory : string -> bool
    val walkWith : string -> (string -> 'a -> 'a) -> 'a -> 'a

    (*
     *  walk: iterates recursively over the given path calling the
     *  handler with the full path of each file.
     *)
    val walk : string -> (string -> unit) -> unit

    (*
     *  which: looks through directories in $PATH to find the file.
     *)
    val which : string -> string option
end
