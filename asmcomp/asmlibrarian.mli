(*s: asmcomp/asmlibrarian.mli *)
(*s: copyright header *)
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
(*e: copyright header *)

(*s: signature Asmlibrarian.create_archive *)
(* Build libraries of .cmx files *)

val create_archive: string list -> string -> unit
(*e: signature Asmlibrarian.create_archive *)

(*s: type Asmlibrarian.error *)
type error =
    File_not_found of string
  | Archiver_error of string
(*e: type Asmlibrarian.error *)

(*s: exception Asmlibrarian.Error *)
exception Error of error
(*e: exception Asmlibrarian.Error *)

(*s: signature Asmlibrarian.report_error *)
val report_error: error -> unit
(*e: signature Asmlibrarian.report_error *)
(*e: asmcomp/asmlibrarian.mli *)
