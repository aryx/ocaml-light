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

(* $Id: reload.ml,v 1.1 1997/07/24 11:49:01 xleroy Exp $ *)

(* Reloading for the HPPA *)

let fundecl f =
  (new Reloadgen.reload_generic ())#fundecl f
