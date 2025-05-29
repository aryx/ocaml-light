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

(* $Id: ocamlmktop.ml,v 1.2 1996/05/11 18:26:49 xleroy Exp $ *)

let _ =
  let args =
    String.concat " " (List.tl (Array.to_list Sys.argv)) in
  exit(Sys.command("ocamlc -linkall toplevellib.cma " ^ args ^ " topmain.cmo"))
