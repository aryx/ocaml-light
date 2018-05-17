(*s: asmcomp/closure.mli *)
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

(*s: signature [[Closure.intro]] *)
(* Introduction of closures, uncurrying, recognition of direct calls *)

val intro: int -> Lambda.lambda -> Clambda.ulambda
(*e: signature [[Closure.intro]] *)

(*e: asmcomp/closure.mli *)
