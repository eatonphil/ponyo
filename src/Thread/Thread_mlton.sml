(* Adapted from http://www.mlton.org/MLtonThread *)
(* TODO: refactor into separate Queue ADT *)

structure Ponyo_Thread_Queue:
   sig
      type 'a t

      val new: unit -> 'a t
      val enque: 'a t * 'a -> unit
      val deque: 'a t -> 'a option
   end =
   struct
      datatype 'a t = T of {front: 'a list ref, back: 'a list ref}

      fun new () = T {front = ref [], back = ref []}

      fun enque (T {back, ...}, x) = back := x :: !back

      fun deque (T {front, back}) =
         case !front of
            [] => (case !back of
                      [] => NONE
                    | l => let val l = rev l
                           in case l of
                              [] => raise Fail "deque"
                            | x :: l => (back := []; front := l; SOME x)
                           end)
          | x :: l => (front := l; SOME x)
   end

structure Ponyo_Thread:
   sig
      val exit: unit -> 'a
      val fork: (unit -> unit) -> unit
      val run: unit -> unit
   end =
   struct
      open Posix.Signal
      open MLton
      open Itimer Signal Thread

      val topLevel: Thread.Runnable.t option ref = ref NONE

      local
         val threads: Thread.Runnable.t Ponyo_Thread_Queue.t = Ponyo_Thread_Queue.new ()
      in
         fun ready (t: Thread.Runnable.t) : unit =
            Ponyo_Thread_Queue.enque(threads, t)
         fun next () : Thread.Runnable.t =
            case Ponyo_Thread_Queue.deque threads of
               NONE => valOf (!topLevel)
             | SOME t => t
      end

      fun 'a exit (): 'a = switch (fn _ => next ())

      fun new (f: unit -> unit): Thread.Runnable.t =
         Thread.prepare
         (Thread.new (fn () => ((f () handle _ => exit ())
                                ; exit ())),
          ())

      fun schedule t = (ready t; next ())

      fun yield (): unit = switch (fn t => schedule (Thread.prepare (t, ())))

      fun fork (f: unit -> unit) : unit = ready (new f)

      fun setItimer t =
         Itimer.set (Itimer.Real,
                     {value = t,
                      interval = t})

      fun run (): unit =
         (switch (fn t =>
                  (topLevel := SOME (Thread.prepare (t, ()))
                   ; new (fn () => (setHandler (alrm, Handler.handler schedule)
                                    ; setItimer (Time.fromMilliseconds 20)))))
          ; setItimer Time.zeroTime
          ; ignore alrm
          ; topLevel := NONE)
   end
