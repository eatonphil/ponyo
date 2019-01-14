structure Ponyo_Format_Template_Test =
struct
    local
        structure Template = Ponyo.Format.Template
        val test = Ponyo.Test.test
    in
        fun run () =
            let
                g = fn (s) => Template.generate (Template.parse s)
            in
                [
                    test "Template.parse" [
                        "a 123 b" = g "a {{ foo }} b" (Dict.fromList [("foo" 123)]),
                        "123" = g "{{ foo }}" (Dict.fromList [("foo" 123)])
                    ]
                ]
            end
    end
end
