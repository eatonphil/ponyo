structure Http = Ponyo.Net.Http;

infix 6 >=>;
fun f >=> g = fn x => g (f (x))

fun main () =
    let
        val req = Request.new (Http.Method.Get, "", "")
        val rsp = Http.Client.act ("api.ipify.org", req)
	val headers = Http.Headers.toList (#headers rsp)
    in
        map (Header.marshall >=> PolyML.print) headers;
        PolyML.print (#version rsp);
	PolyML.print (#status rsp);
	PolyML.print (#reason rsp);
        print (#body rsp);
	()
    end
