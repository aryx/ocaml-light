(*s: bytecomp/bytegen.mli *)
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

(* Generation of bytecode from lambda terms *)

open Lambda
open Instruct

(*s: signature [[Bytegen.compile_implementation]] *)
val compile_implementation: string -> lambda -> instruction list
(*e: signature [[Bytegen.compile_implementation]] *)
(*s: signature [[Bytegen.compile_phrase]] *)
val compile_phrase: lambda -> instruction list * instruction list
(*e: signature [[Bytegen.compile_phrase]] *)
(*e: bytecomp/bytegen.mli *)
