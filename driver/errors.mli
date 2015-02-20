(*s: ./driver/errors.mli *)
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

(* $Id: errors.mli,v 1.3 1996/04/30 14:45:55 xleroy Exp $ *)

(*s: signature Errors.report_error *)
(* Error report *)

val report_error: exn -> unit
(*e: signature Errors.report_error *)
(*e: ./driver/errors.mli *)
