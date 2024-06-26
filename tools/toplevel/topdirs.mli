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

(* $Id: topdirs.mli,v 1.7 1997/07/03 14:32:30 xleroy Exp $ *)

(* The toplevel directives. *)

val dir_quit : unit -> unit
val dir_directory : string -> unit
val dir_cd : string -> unit
val dir_load : string -> unit
val dir_use : string -> unit
val dir_install_printer : Longident.t -> unit
val dir_remove_printer : Longident.t -> unit
val dir_trace : Longident.t -> unit
val dir_untrace : Longident.t -> unit
val dir_untrace_all : unit -> unit

