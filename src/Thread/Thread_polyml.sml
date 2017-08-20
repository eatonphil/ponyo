structure Ponyo_Thread =
struct
    open Thread.Thread

    fun fork (f) = Thread.Thread.fork (f, [])
    fun run () = ()
end