(*s: asmcomp/printcmm.mli *)
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

(* $Id: printcmm.mli,v 1.4 1996/04/30 14:42:43 xleroy Exp $ *)

(*s: signature Printcmm.machtype_component *)
(* Pretty-printing of C-- code *)

val machtype_component : Cmm.machtype_component -> unit
(*e: signature Printcmm.machtype_component *)
(*s: signature Printcmm.machtype *)
val machtype : Cmm.machtype_component array -> unit
(*e: signature Printcmm.machtype *)
(*s: signature Printcmm.comparison *)
val comparison : Cmm.comparison -> unit
(*e: signature Printcmm.comparison *)
(*s: signature Printcmm.chunk *)
val chunk : Cmm.memory_chunk -> unit
(*e: signature Printcmm.chunk *)
(*s: signature Printcmm.operation *)
val operation : Cmm.operation -> unit
(*e: signature Printcmm.operation *)
(*s: signature Printcmm.expression *)
val expression : Cmm.expression -> unit
(*e: signature Printcmm.expression *)
(*s: signature Printcmm.fundecl *)
val fundecl : Cmm.fundecl -> unit
(*e: signature Printcmm.fundecl *)
(*s: signature Printcmm.data *)
val data : Cmm.data_item list -> unit
(*e: signature Printcmm.data *)
(*s: signature Printcmm.phrase *)
val phrase : Cmm.phrase -> unit
(*e: signature Printcmm.phrase *)
(*e: asmcomp/printcmm.mli *)
