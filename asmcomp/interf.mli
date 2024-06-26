(*s: asmcomp/interf.mli *)
(*s: copyright header *)
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
(*e: copyright header *)

(*s: signature [[Interf.build_graph]] *)
(* Construction of the interference graph.
   Annotate pseudoregs with interference lists and preference lists. *)

val build_graph: Mach.fundecl -> unit
(*e: signature [[Interf.build_graph]] *)
(*e: asmcomp/interf.mli *)
