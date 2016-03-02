structure Method =
struct
    local structure String = StringExport in

    datatype t =
        Connect
      | Delete
      | Get
      | Head
      | Options
      | Post
      | Put
      | Trace
      | Unknown of string

    fun fromString (v: string) : t = case String.toUpper(v) of
        "CONNECT" => Connect
      |	"DELETE"  => Delete
      | "GET"     => Get
      | "HEAD"    => Head
      | "OPTIONS" => Options
      | "POST"    => Post
      | "PUT"     => Put
      | "TRACE"   => Trace
      | _         => Unknown (v)

    fun toString v = case v of
        Connect   => "CONNECT"
      | Delete    => "DELETE"
      | Get       => "GET"
      | Head      => "HEAD"
      | Options   => "OPTIONS"
      | Post      => "POST"
      | Put       => "PUT"
      | Trace     => "TRACE"
      | Unknown v => String.toUpper(v)
    end
end
