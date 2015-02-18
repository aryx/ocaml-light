(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: exec.mli,v 1.2 1997/11/13 09:04:13 vouillon Exp $ *)

(* Handling of keyboard interrupts *)

val protect : (unit -> unit) -> unit
val unprotect : (unit -> unit) -> unit
