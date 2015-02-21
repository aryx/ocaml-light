(*s: asmcomp/asmlink.mli *)
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

(* $Id: asmlink.mli,v 1.4 1996/04/30 14:42:11 xleroy Exp $ *)

(*s: signature Asmlink.link *)
(* Link a set of .cmx/.o files and produce an executable *)

val link: string list -> unit
(*e: signature Asmlink.link *)

(*s: type Asmlink.error *)
type error =
    File_not_found of string
  | Not_an_object_file of string
  | Missing_implementations of string list
  | Inconsistent_interface of string * string * string
  | Inconsistent_implementation of string * string * string
  | Assembler_error of string
  | Linking_error
(*e: type Asmlink.error *)

(*s: exception Asmlink.Error *)
exception Error of error
(*e: exception Asmlink.Error *)

(*s: signature Asmlink.report_error *)
val report_error: error -> unit
(*e: signature Asmlink.report_error *)
(*e: asmcomp/asmlink.mli *)
