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

(* $Id: loadprinter.mli,v 1.1 1997/02/14 16:29:58 xleroy Exp $ *)

(* Loading and installation of user-defined printer functions *)

val loadfile : string -> unit
val install_printer : Longident.t -> unit
val remove_printer : Longident.t -> unit

(* Error report *)

type error =
    Load_failure of Dynlink.error
  | Unbound_identifier of Longident.t
  | Unavailable_module of string * Longident.t
  | Wrong_type of Longident.t
  | No_active_printer of Longident.t

exception Error of error

val report_error: error -> unit
