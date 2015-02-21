(*s: asmcomp/cmmgen.mli *)
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

(* $Id: cmmgen.mli,v 1.7 1997/07/02 18:14:37 xleroy Exp $ *)

(*s: signature Cmmgen.compunit *)
(* Translation from closed lambda to C-- *)

val compunit: int -> Clambda.ulambda -> Cmm.phrase list
(*e: signature Cmmgen.compunit *)

(*s: signature Cmmgen.apply_function *)
val apply_function: int -> Cmm.phrase
(*e: signature Cmmgen.apply_function *)
(*s: signature Cmmgen.curry_function *)
val curry_function: int -> Cmm.phrase list
(*e: signature Cmmgen.curry_function *)
(*s: signature Cmmgen.entry_point *)
val entry_point: string list -> Cmm.phrase
(*e: signature Cmmgen.entry_point *)
(*s: signature Cmmgen.global_table *)
val global_table: string list -> Cmm.phrase
(*e: signature Cmmgen.global_table *)
(*s: signature Cmmgen.frame_table *)
val frame_table: string list -> Cmm.phrase
(*e: signature Cmmgen.frame_table *)
(*s: signature Cmmgen.data_segment_table *)
val data_segment_table: string list -> Cmm.phrase
(*e: signature Cmmgen.data_segment_table *)
(*s: signature Cmmgen.code_segment_table *)
val code_segment_table: string list -> Cmm.phrase
(*e: signature Cmmgen.code_segment_table *)
(*s: signature Cmmgen.predef_exception *)
val predef_exception: string -> Cmm.phrase
(*e: signature Cmmgen.predef_exception *)
(*e: asmcomp/cmmgen.mli *)
