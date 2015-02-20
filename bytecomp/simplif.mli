(*s: ./bytecomp/simplif.mli *)
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

(* $Id: simplif.mli,v 1.2 1996/04/30 14:44:20 xleroy Exp $ *)

(* Elimination of useless Llet(Alias) bindings *)

open Lambda

(*s: signature Simplif.simplify_lambda *)
val simplify_lambda: lambda -> lambda
(*e: signature Simplif.simplify_lambda *)
(*e: ./bytecomp/simplif.mli *)
