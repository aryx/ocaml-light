(*s: asmcomp/spill.mli *)
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

(*s: signature Spill.fundecl *)
(* Insertion of moves to suggest possible spilling / reloading points 
   before register allocation. *)

val fundecl: Mach.fundecl -> Mach.fundecl
(*e: signature Spill.fundecl *)
(*e: asmcomp/spill.mli *)
