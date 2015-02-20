(*s: ./bytecomp/bytelibrarian.mli *)
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

(* $Id: bytelibrarian.mli,v 1.4 1996/04/30 14:44:02 xleroy Exp $ *)

(* Build libraries of .cmo files *)

(*s: signature Bytelibrarian.create_archive *)
(* Format of a library file:
      magic number (Config.cma_magic_number)
      absolute offset of content table
      blocks of relocatable bytecode
      content table = list of compilation units
*)

val create_archive: string list -> string -> unit
(*e: signature Bytelibrarian.create_archive *)

(*s: type Bytelibrarian.error *)
type error =
    File_not_found of string
  | Not_an_object_file of string
(*e: type Bytelibrarian.error *)

(*s: exception Bytelibrarian.Error *)
exception Error of error
(*e: exception Bytelibrarian.Error *)

(*s: signature Bytelibrarian.report_error *)
val report_error: error -> unit
(*e: signature Bytelibrarian.report_error *)
(*e: ./bytecomp/bytelibrarian.mli *)
