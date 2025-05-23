external mycallback1 : ('a -> 'b) -> 'a -> 'b = "mycallback1"
external mycallback2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c = "mycallback2"
external mycallback3 : ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd = "mycallback3"

let rec tak (x, y, z as tuple) =
  if x > y then tak(tak (x-1, y, z), tak (y-1, z, x), tak (z-1, x, y))
           else z

let tak2 x (y, z) = tak (x, y, z)

let tak3 x y z = tak (x, y, z)

let raise_exit () = (raise Exit : unit)

let trapexit () =
  begin try
    mycallback1 raise_exit ()
  with Exit ->
    ()
  end;
  tak (18, 12, 6)

external mypushroot : 'a -> ('b -> 'c) -> 'b -> 'a = "mypushroot"

let tripwire () =
  let s = String.make 5 'a' in
  mypushroot s trapexit ()

(* Test callbacks performed to handle signals *)

let sighandler signo =
  print_string "Got signal, triggering garbage collection...";
  print_newline();
  (* Thoroughly wipe the minor heap *)
  tak (18, 12, 6);
  ()

external unix_getpid : unit -> int = "unix_getpid" "noalloc"
external unix_kill : int -> int -> unit = "unix_kill" "noalloc"

let callbacksig () =
  let pid = unix_getpid() in
  (* Allocate a block in the minor heap *)
  let s = String.make 5 'b' in
  (* Send a signal to self.  We want s to remain in a register and
     not be spilled on the stack, hence we declare unix_kill
     "noalloc". *)
  unix_kill pid Sys.sigusr1;
  (* Allocate some more so that the signal will be tested *)
  let u = (s, s) in
  fst u

let _ =
  print_int(mycallback1 tak (18, 12, 6)); print_newline();
  print_int(mycallback2 tak2 18 (12, 6)); print_newline();
  print_int(mycallback3 tak3 18 12 6); print_newline();
  print_int(trapexit ()); print_newline();
  print_string(tripwire ()); print_newline();
  Sys.signal Sys.sigusr1 (Sys.Signal_handle sighandler);
  print_string(callbacksig ()); print_newline()

