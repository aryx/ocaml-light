(*s: asmcomp/selection.mli *)
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

(*s: signature Selection.fundecl *)
(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

val fundecl: Cmm.fundecl -> Mach.fundecl
(*e: signature Selection.fundecl *)
(*e: asmcomp/selection.mli *)
