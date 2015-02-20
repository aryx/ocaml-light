(*s: ./parsing/syntaxerr.mli *)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: syntaxerr.mli,v 1.2 1997/11/12 12:32:53 xleroy Exp $ *)

(*s: type Syntaxerr.error *)
(* Auxiliary type for reporting syntax errors *)

type error =
    Unclosed of Location.t * string * Location.t * string
  | Other of Location.t
(*e: type Syntaxerr.error *)

(*s: exception Syntaxerr.Error *)
exception Error of error
(*e: exception Syntaxerr.Error *)
(*s: exception Syntaxerr.Escape_error *)
exception Escape_error
(*e: exception Syntaxerr.Escape_error *)

(*s: signature Syntaxerr.report_error *)
val report_error: error -> unit
(*e: signature Syntaxerr.report_error *)
(*e: ./parsing/syntaxerr.mli *)
