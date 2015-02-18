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

(* $Id: runtimedef.mli,v 1.3 1996/04/30 14:44:18 xleroy Exp $ *)

(* Values and functions known and/or provided by the runtime system *)

val builtin_exceptions: string array
val builtin_primitives: string array
