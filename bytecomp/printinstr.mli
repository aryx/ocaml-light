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

(* $Id: printinstr.mli,v 1.3 1996/04/30 14:44:15 xleroy Exp $ *)

(* Pretty-print lists of instructions *)

open Instruct

val instruction: instruction -> unit
val instrlist: instruction list -> unit
