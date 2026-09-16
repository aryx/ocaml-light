let x = [1;2;3]

let f x = 1 :: 2 :: 3 :: x

let _ =
  if x = f [] then print_string "OK " else print_string "failed! ";
  (* claude: != (physical), not <> (structural) -- stdout/stderr are
     Custom_tag blocks with no compare function defined, so structural
     comparison of two different channels always raises
     Failure("equal: abstract value") by design (see
     byterun/custom.c's custom_compare_default); this test almost
     certainly meant physical inequality *)
  if stdout != stderr then print_string "OK" else print_string "failed!";
  print_newline()
