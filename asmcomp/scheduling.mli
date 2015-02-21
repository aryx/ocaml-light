(*s: asmcomp/scheduling.mli *)
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

(* $Id: scheduling.mli,v 1.2 1996/04/30 14:42:59 xleroy Exp $ *)

(*s: signature Scheduling.fundecl *)
(* Instruction scheduling *)

val fundecl: Linearize.fundecl -> Linearize.fundecl
(*e: signature Scheduling.fundecl *)
(*e: asmcomp/scheduling.mli *)
