(*s: asmcomp/liveness.mli *)
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

(*s: signature Liveness.fundecl *)
(* Liveness analysis.
   Annotate mach code with the set of regs live at each point. *)

val fundecl: Mach.fundecl -> unit
(*e: signature Liveness.fundecl *)
(*e: asmcomp/liveness.mli *)
