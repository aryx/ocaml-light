(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parameters.mli,v 1.1 1996/11/29 16:55:04 xleroy Exp $ *)

(* Miscellaneous parameters *)

val program_name : string ref
val socket_name : string ref
val arguments : string ref
val default_load_path : string list ref

val add_path : string -> unit

(* Used by emacs ? *)
val emacs : bool ref
