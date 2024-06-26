(*s: asmcomp/asmgen.mli *)
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

(*s: signature [[Asmgen.compile_implementation]] *)
(* From lambda to assembly code *)

val compile_implementation: string -> int -> Lambda.lambda -> unit
(*e: signature [[Asmgen.compile_implementation]] *)
(*s: signature [[Asmgen.compile_phrase]] *)
val compile_phrase: Cmm.phrase -> unit
(*e: signature [[Asmgen.compile_phrase]] *)

(*s: type [[Asmgen.error]] *)
type error = Assembler_error of string
(*e: type [[Asmgen.error]] *)
(*s: exception [[Asmgen.Error]] *)
exception Error of error
(*e: exception [[Asmgen.Error]] *)
(*s: signature [[Asmgen.report_error]] *)
val report_error: error -> unit
(*e: signature [[Asmgen.report_error]] *)


(*e: asmcomp/asmgen.mli *)
