infix 6 >=>;
fun f >=> g = fn x => g (f (x))

fun main () =
    let
        val req = (Http.Method.Get, "http://api.ipify.org", "")
        val rsp = (Http.Request.new >=> Http.Client.act) req
	val headers = Http.Headers.toList (#headers rsp)
    in
        map (Header.marshall >=> PolyML.print) headers;
        PolyML.print (#version rsp);
	PolyML.print (#status rsp);
	PolyML.print (#reason rsp);
        print (#body rsp);
	()
    end
