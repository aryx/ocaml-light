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

(* $Id: int_misc.mli,v 1.2 1996/04/30 14:47:03 xleroy Exp $ *)

(* Some extra operations on integers *)

val gcd_int: int -> int -> int
val num_bits_int: int -> int
val compare_int: int -> int -> int
val sign_int: int -> int
val length_of_int: int
val biggest_int: int
val least_int: int
val monster_int: int
