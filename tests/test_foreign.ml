
external test_foreign: int -> int -> unit = "caml_test_foreign"
external test_foreign2: unit -> unit = "caml_test_foreign2"

(* export CAMLRUNPARAM=b and then run this program.
 * This used to cause some segfaults
 *)

let foo () =
  let _ = test_foreign2 () in
  ()


let test () =
  Printf.printf "Before\n";
  test_foreign 2 4;
  let _ = Array.make 10000 (Some 2) in
  Gc.minor();
  Printf.printf "After\n";
  (try 
    foo ();
  with e ->
    let s = Printexc.get_backtrace () in
    print_string s;
    Printf.printf "Really After\n";
  )


let _ =
  let r = Gc.get () in
  r.Gc.verbose <- true;
  Gc.set r;
  test ()
