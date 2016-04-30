structure Ponyo_Net_Http_Headers =
struct
    local
        structure Option = Ponyo_Container_Option
        structure String = Ponyo_String
        structure Header = Ponyo_Net_Http_Header
    in

    structure Headers = Ponyo_Container_Map (String);

    open Headers;

    fun get (headers: string Headers.t) (name: string) : Header.t option =
        Option.>>= ((Headers.get headers name),
                    (fn (value) => Header.fromKv (name, value)))

    fun toList (headers: string Headers.t) : Header.t list =
        map Header.fromKv (Headers.toList headers)

    end
end
