(*s: asmcomp/printlinear.mli *)
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

(* $Id: printlinear.mli,v 1.3 1996/04/30 14:42:44 xleroy Exp $ *)

(* Pretty-printing of linearized machine code *)

open Linearize

(*s: signature Printlinear.instr *)
val instr: instruction -> unit
(*e: signature Printlinear.instr *)
(*s: signature Printlinear.fundecl *)
val fundecl: fundecl -> unit
(*e: signature Printlinear.fundecl *)
(*e: asmcomp/printlinear.mli *)
