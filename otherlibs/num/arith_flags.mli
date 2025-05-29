(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*    Valerie Menissier-Morain, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: arith_flags.mli,v 1.3 1996/04/30 14:46:59 xleroy Exp $ *)

val error_when_null_denominator_flag : bool ref
val normalize_ratio_flag : bool ref
val normalize_ratio_when_printing_flag : bool ref
val floating_precision : int ref
val approx_printing_flag : bool ref
