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
        map print (String.split ("https://www.google.com/", "//"));
        print "\n\n";
        print (String.join (String.split (String.join (["foobar\n", "\n", "bar\n"], ""), "\n"), "\n"));
        print "\n\n";
        map PolyML.print (String.split ("structure F : S =", "structure"));
        PolyML.print (String.join (["1", "2"], "b"));
        PolyML.print (String.replace ("foo\nbar", "\n", " "));
        ()
    end
