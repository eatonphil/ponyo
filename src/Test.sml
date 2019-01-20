structure Ponyo_Test =
struct
    local
        structure Format = Ponyo_Format_String
        structure Terminal = Ponyo_Os_Terminal
    in
        fun test (description: string) (testResults: bool list) : bool =
            let
                fun testsFailed (testResults: bool list, current: int, failed: int list) : int list =
                    case testResults of
                        [] => List.rev (failed)
                      | test :: rest => testsFailed (rest, current + 1, if test then failed else current + 1 :: failed)

                val total = length testResults
                val failed = testsFailed (testResults, 0, [])
                val successful = total - (length failed)
                val failText = Terminal.colorize "Test % failed.\r\n" Terminal.Red
            in
                Format.println ["Testing " ^ Terminal.bold description];
                map (fn t => Format.printf failText [Int.toString t]) failed;
                let
                    val [s, t] = map Int.toString [successful, total]
                    val color = if successful = total then Terminal.Green else Terminal.Red
                    val text = Terminal.colorize "% of % tests successful.\r\n\r\n" color
                in
                    Format.printf text [s, t]
                end;
                successful = total
            end
    end
end
