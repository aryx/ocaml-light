
external test_foreign: int -> int -> unit = "caml_test_foreign"

let test () =
  Printf.printf "Before\n";
  test_foreign 2 4;
  let _ = Array.make 10000 (Some 2) in
  Gc.minor();
  Printf.printf "After\n";
  ()


let _ =
  let r = Gc.get () in
  r.Gc.verbose <- true;
  Gc.set r;
  test ()
