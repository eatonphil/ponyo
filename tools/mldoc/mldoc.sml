structure File = Ponyo.Os.File

fun parseModule (modulePath: string) =
    let
        val module = File.read (modulePath)
    in
        
    end

fun main () =
    parseModule (hd (CommandLine.arguments ()))
