(*s: ./driver/opterrors.mli *)
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

(* $Id: opterrors.mli,v 1.3 1996/04/30 14:45:59 xleroy Exp $ *)

(*s: signature Opterrors.report_error *)
(* Error report *)

val report_error: exn -> unit
(*e: signature Opterrors.report_error *)
(*e: ./driver/opterrors.mli *)
