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

(* $Id: optcompile.mli,v 1.5 1996/04/30 14:45:57 xleroy Exp $ *)

(* Compile a .ml or .mli file *)

val interface: string -> unit
val implementation: string -> unit
val c_file: string -> unit

val initial_env: unit -> Env.t
val init_path: unit -> unit
