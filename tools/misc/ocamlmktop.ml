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


let _ =
  let args =
    String.concat " " (List.tl (Array.to_list Sys.argv)) in
  exit(Sys.command("ocamlc -linkall toplevellib.cma " ^ args ^ " topmain.cmo"))
