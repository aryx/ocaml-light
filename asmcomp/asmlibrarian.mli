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

(* $Id: asmlibrarian.mli,v 1.4 1996/04/30 14:42:09 xleroy Exp $ *)

(* Build libraries of .cmx files *)

val create_archive: string list -> string -> unit

type error =
    File_not_found of string
  | Archiver_error of string

exception Error of error

val report_error: error -> unit
