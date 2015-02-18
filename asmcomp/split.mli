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

(* $Id: split.mli,v 1.3 1996/04/30 14:43:05 xleroy Exp $ *)

(* Renaming of registers at reload points to split live ranges. *)

val fundecl: Mach.fundecl -> Mach.fundecl
