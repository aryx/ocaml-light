let _ =
  print_string "hello world\n"

type record = {
 x: int;
 y: int;
}

let r1 = { x = 1; y = 2 }
let r2 = { r1 with y = 3 }

let _ =
  print_int r2.y;
  print_endline ""
