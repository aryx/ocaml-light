(*s: ./bytecomp/bytelink.mli *)
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

(*s: signature Bytelink.link *)
(* Link .cmo files and produce a bytecode executable. *)

val link: string list -> unit
(*e: signature Bytelink.link *)

(*s: signature Bytelink.check_consistency *)
val check_consistency: string -> Emitcode.compilation_unit -> unit
(*e: signature Bytelink.check_consistency *)

(*s: type Bytelink.error *)
type error =
    File_not_found of string
  | Not_an_object_file of string
  | Symbol_error of string * Symtable.error
  | Inconsistent_import of string * string * string
  | Custom_runtime
  | File_exists of string
(*e: type Bytelink.error *)

(*s: exception Bytelink.Error *)
exception Error of error
(*e: exception Bytelink.Error *)

(*s: signature Bytelink.report_error *)
val report_error: error -> unit
(*e: signature Bytelink.report_error *)
(*e: ./bytecomp/bytelink.mli *)
