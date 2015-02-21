(*s: asmcomp/emit.mli *)
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

(* $Id: emit.mli,v 1.4 1996/04/30 14:42:24 xleroy Exp $ *)

(*s: signature Emit.fundecl *)
(* Generation of assembly code *)

val fundecl: Linearize.fundecl -> unit
(*e: signature Emit.fundecl *)
(*s: signature Emit.data *)
val data: Cmm.data_item list -> unit
(*e: signature Emit.data *)
(*s: signature Emit.begin_assembly *)
val begin_assembly: unit -> unit
(*e: signature Emit.begin_assembly *)
(*s: signature Emit.end_assembly *)
val end_assembly: unit -> unit
(*e: signature Emit.end_assembly *)
(*e: asmcomp/emit.mli *)
