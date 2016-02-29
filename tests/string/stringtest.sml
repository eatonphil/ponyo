fun main () = let in
        PolyML.print (String.indexOf ("foobar", "bar"));
	PolyML.print (String.indexOf ("/www.google.com/", "/"));
	map PolyML.print (String.split ("Header: foo", ":"));
	PolyML.print (length (String.splitN ("Header bar foo 1", " ", 1)));
	PolyML.print (String.stripAll ("\t    Header \t ", [" ", "\t"]));
	PolyML.print (String.count ("///https://www.google.com//", "//"));
        map PolyML.print (String.split ("https://www.google.com/", "//"));
        ()
    end
