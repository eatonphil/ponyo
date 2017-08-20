fun debug (s: string) =
    let in
        print (s);
        TextIO.flushOut (TextIO.stdOut)
    end

structure Ponyo = struct
    structure Char = Ponyo_Char
    structure Encoding = Ponyo_Encoding
    structure Format = Ponyo_Format
    structure Int = Ponyo_Int
    structure Net = Ponyo_Net
    structure Os = Ponyo_Os
    structure Sml = Ponyo_Sml
    structure String = Ponyo_String
    structure Test = Ponyo_Test
    structure Thread = Ponyo_Thread
end