fun main () = let in
        PolyML.print (String.indexOf ("foobar", "bar"));
	PolyML.print (String.indexOf ("/www.google.com/", "/"));
        map PolyML.print (String.split ("https://www.google.com/", "//"));
        ()
    end
