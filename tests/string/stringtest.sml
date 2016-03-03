structure String = Ponyo.String

fun main () =
    let in
        PolyML.print (String.substring ("foobar", 0, 3));
        PolyML.print (String.indexOf ("foobar", "bar"));
	PolyML.print (String.indexOf ("/www.google.com/", "/"));
	map PolyML.print (String.split ("Header: foo", ":"));
	PolyML.print (length (String.splitN ("Header bar foo 1", " ", 1)));
	PolyML.print (String.stripAll ("\t    Header \t ", [" ", "\t"]));
	PolyML.print (String.count ("///https://www.google.com//", "//"));
        map PolyML.print (String.split ("https://www.google.com/", "//"));
        PolyML.print (String.join (["1", "2"], "b"));
        ()
    end
