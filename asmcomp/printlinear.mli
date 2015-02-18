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

val instr: instruction -> unit
val fundecl: fundecl -> unit
