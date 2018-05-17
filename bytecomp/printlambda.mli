(*s: ./bytecomp/printlambda.mli *)
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

open Lambda

(*s: signature [[Printlambda.structured_constant]] *)
val structured_constant: structured_constant -> unit
(*e: signature [[Printlambda.structured_constant]] *)
(*s: signature [[Printlambda.lambda]] *)
val lambda: lambda -> unit
(*e: signature [[Printlambda.lambda]] *)
(*e: ./bytecomp/printlambda.mli *)
