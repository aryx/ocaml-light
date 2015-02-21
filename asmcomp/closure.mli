(*s: asmcomp/closure.mli *)
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

(* $Id: closure.mli,v 1.4 1996/04/30 14:42:14 xleroy Exp $ *)

(*s: signature Closure.intro *)
(* Introduction of closures, uncurrying, recognition of direct calls *)

val intro: int -> Lambda.lambda -> Clambda.ulambda
(*e: signature Closure.intro *)

(*e: asmcomp/closure.mli *)
