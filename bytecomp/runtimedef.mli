(*s: ./bytecomp/runtimedef.mli *)
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

(*s: signature [[Runtimedef.builtin_exceptions]] *)
(* Values and functions known and/or provided by the runtime system *)

val builtin_exceptions: string array
(*e: signature [[Runtimedef.builtin_exceptions]] *)
(*s: signature [[Runtimedef.builtin_primitives]] *)
val builtin_primitives: string array
(*e: signature [[Runtimedef.builtin_primitives]] *)
(*e: ./bytecomp/runtimedef.mli *)
