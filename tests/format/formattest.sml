structure Format = Ponyo.Format

fun main () =
    let in
	Format.printf "%\n" (Format.str "12/24/16", Format.last) [#1];
        Format.println Format.null [];
	Format.printf "[%]: %\n" (Format.str "12/24/16", Format.str "ERROR") [#1, #2];
        Format.println (Format.int 1, Format.str "s") [#1, #2]
    end
