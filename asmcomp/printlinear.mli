(*s: asmcomp/printlinear.mli *)
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

(* Pretty-printing of linearized machine code *)

open Linearize

(*s: signature Printlinear.instr *)
val instr: instruction -> unit
(*e: signature Printlinear.instr *)
(*s: signature Printlinear.fundecl *)
val fundecl: fundecl -> unit
(*e: signature Printlinear.fundecl *)
(*e: asmcomp/printlinear.mli *)
