structure Ponyo_Test =
struct
    local
        structure Format = Ponyo_Format
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
            in
                Format.printf "Testing %\r\n" [description];
                map (fn t => Format.printf "Test % failed.\r\n" [Int.toString t]) failed;
                Format.printf "% of % tests successful.\r\n\r\n" (map Int.toString [successful, total]);
                successful = total
            end
    end
end
