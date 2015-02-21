(*s: asmcomp/interf.mli *)
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

(* $Id: interf.mli,v 1.3 1996/04/30 14:42:35 xleroy Exp $ *)

(*s: signature Interf.build_graph *)
(* Construction of the interference graph.
   Annotate pseudoregs with interference lists and preference lists. *)

val build_graph: Mach.fundecl -> unit
(*e: signature Interf.build_graph *)
(*e: asmcomp/interf.mli *)
