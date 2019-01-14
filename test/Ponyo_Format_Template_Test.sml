structure Ponyo_Format_Template_Test =
struct
    local
        structure Template = Ponyo.Format.Template
        structure Dict = Ponyo.String.Dict
        val test = Ponyo.Test.test
    in
        fun run () =
            let
                val g = fn (s) => Template.generate (#1 (Template.parse s))
            in
                [
                    test "Template.parse" [
                        "a 123 b" = g "a {{ foo }} b" (Dict.fromList [("foo", "123")]),
                        "123" = g "{{ foo }}" (Dict.fromList [("foo", "123")])
                    ]
                ]
            end
    end
end
