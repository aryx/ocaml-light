(*s: asmcomp/selection.mli *)
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

(* $Id: selection.mli,v 1.5 1996/04/30 14:43:01 xleroy Exp $ *)

(*s: signature Selection.fundecl *)
(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

val fundecl: Cmm.fundecl -> Mach.fundecl
(*e: signature Selection.fundecl *)
(*e: asmcomp/selection.mli *)
