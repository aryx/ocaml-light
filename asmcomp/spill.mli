(*s: asmcomp/spill.mli *)
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

(* $Id: spill.mli,v 1.3 1996/04/30 14:43:03 xleroy Exp $ *)

(*s: signature Spill.fundecl *)
(* Insertion of moves to suggest possible spilling / reloading points 
   before register allocation. *)

val fundecl: Mach.fundecl -> Mach.fundecl
(*e: signature Spill.fundecl *)
(*e: asmcomp/spill.mli *)
