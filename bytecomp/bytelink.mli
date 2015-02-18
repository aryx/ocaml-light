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

(* $Id: bytelink.mli,v 1.4 1996/11/07 10:56:52 xleroy Exp $ *)

(* Link .cmo files and produce a bytecode executable. *)

val link: string list -> unit

val check_consistency: string -> Emitcode.compilation_unit -> unit

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Symbol_error of string * Symtable.error
  | Inconsistent_import of string * string * string
  | Custom_runtime
  | File_exists of string

exception Error of error

val report_error: error -> unit
