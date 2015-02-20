(*s: ./bytecomp/printlambda.mli *)
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

(* $Id: printlambda.mli,v 1.3 1996/04/30 14:44:17 xleroy Exp $ *)

open Lambda

(*s: signature Printlambda.structured_constant *)
val structured_constant: structured_constant -> unit
(*e: signature Printlambda.structured_constant *)
(*s: signature Printlambda.lambda *)
val lambda: lambda -> unit
(*e: signature Printlambda.lambda *)
(*e: ./bytecomp/printlambda.mli *)
