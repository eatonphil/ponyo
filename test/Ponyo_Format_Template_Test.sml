structure Ponyo_Format_Template_Test =
struct
    local
        structure Template = Ponyo.Format.Template
        structure Dict = Ponyo.String.Dict
        val test = Ponyo.Test.test
    in
        fun run () = [
            test "Template.parse" let
                val c = Dict.fromList [("foo", "123")]
                val g = fn (s) => Template.generate (#1 (Template.parse s)) c
            in [
                "123" = g "{{ foo }}",
                "a 123" = g "a {{ foo }}",
                "a 123 b" = g "a {{ foo }} b"
            ] end
        ]
    end
end
