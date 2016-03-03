structure Format = Ponyo.Format

fun main () =
    let in
	Format.printf "%\n" ["12/24/16"];
        Format.println [];
	Format.printf "[%]: %\n" ["12/24/16", "ERROR"];
        Format.println [Format.int 1, "s"]
    end
