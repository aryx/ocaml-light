(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


(* Print the digests of unit interfaces *)

let load_path = ref ["."]
let first = ref true

let print_crc unit =
  try
    let crc = Dynlink.digest_interface unit !load_path in
    if !first then first := false else print_string ";\n";
    print_string "  \""; print_string unit; print_string "\",\n    \"";
    for i = 0 to String.length crc - 1 do
      Printf.printf "\\%03d" (Char.code crc.[i])
    done;
    print_string "\""
  with exn ->
    prerr_string "Error while reading the interface for ";
    prerr_endline unit;
    begin match exn with
      Sys_error msg -> prerr_endline msg
    | Dynlink.Error _ -> prerr_endline "Ill formed .cmi file"
    | _ -> raise exn
    end;
    exit 2

let usage = "Usage: extract_crc [-I <dir>] <files>"

let main () =
  print_string "let crc_unit_list = [\n";
  Arg.parse
    ["-I", Arg.String(fun dir -> load_path := !load_path @ [dir]),
           "<dir>  Add <dir> to the list of include directories"]
    print_crc usage;
  print_string "\n]\n"

let _ = main(); exit 0

     
