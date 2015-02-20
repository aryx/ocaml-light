(*s: ./bytecomp/simplif.mli *)
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

(* Elimination of useless Llet(Alias) bindings *)

open Lambda

(*s: signature Simplif.simplify_lambda *)
val simplify_lambda: lambda -> lambda
(*e: signature Simplif.simplify_lambda *)
(*e: ./bytecomp/simplif.mli *)
