val req : Http.Request.t = {
    method   = Http.Method.Get,
    domain   = "api.ipify.org",
    path     = "/",
    port     = 80,
    headers  = [Http.Header.Host "api.ipify.org"],
    body     = ""
}

fun main () =
    let
        val rsp = Http.Client.act (req)
    in
        map (fn h => PolyML.print (Http.Header.marshal (h))) (#headers rsp);
	PolyML.print (#version rsp);
	PolyML.print (#status rsp);
	PolyML.print (#reason rsp);
        print (#body rsp);
	()
    end
