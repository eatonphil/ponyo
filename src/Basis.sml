structure Basis = struct
    structure String = String
    structure List = List
    structure OS = OS
    structure Posix = Posix
    structure Socket = Socket
    structure Int = Int
    structure Char = Char
    structure Time = Time
end

fun debug (s: string) =
    let in
        print (s);
        TextIO.flushOut (TextIO.stdOut)
    end