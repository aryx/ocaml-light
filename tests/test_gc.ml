let foo x =
  if x = 1
  then 
    let _ = failwith "Exn" in
    3
  else 2

let bar x =
  if x = 1
  then 2 + foo x
  else 3 + foo x

let foobar x =
  try 
    if x = 2
    then bar x + 3
    else bar x + 4
  with
  | Not_found -> 1
  | e -> 
    Gc.compact();
    let s = Printexc.get_backtrace () in
    print_string s;
    Gc.compact();
    (*raise e *)
    2

let main () =
  foobar 1 + 
  45

let _ = main()
