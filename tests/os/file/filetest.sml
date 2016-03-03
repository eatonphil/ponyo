structure File = Ponyo.Os.File
structure Format = Ponyo.Format
structure String = Ponyo.String

fun main () =
    let
        val fileName = "tests/os/file/helloworld.txt"
        val file = File.use (fileName, File.Read)
    in
        Format.println [String.join (File.read file, "")];
    	File.close (file);
        Format.println [];
        Format.println [String.join (File.readFrom fileName, "")]
    end
