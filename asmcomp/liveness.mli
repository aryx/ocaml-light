(*s: asmcomp/liveness.mli *)
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

(* $Id: liveness.mli,v 1.3 1996/04/30 14:42:39 xleroy Exp $ *)

(*s: signature Liveness.fundecl *)
(* Liveness analysis.
   Annotate mach code with the set of regs live at each point. *)

val fundecl: Mach.fundecl -> unit
(*e: signature Liveness.fundecl *)
(*e: asmcomp/liveness.mli *)
