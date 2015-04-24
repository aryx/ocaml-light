(*s: asmcomp/arm/reload.ml *)
(*s: copyright header 1998 *)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header 1998 *)

open Reloadgen

(*s: function Reload.fundecl *)
(* Reloading for the ARM *)

let fundecl f =
  let reloader = Reloadgen.reload_generic () in
  reloader.fundecl reloader f
(*e: function Reload.fundecl *)
(*e: asmcomp/arm/reload.ml *)
