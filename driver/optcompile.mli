(*s: ./driver/optcompile.mli *)
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

(*s: signature Optcompile.interface *)
(* Compile a .ml or .mli file *)

val interface: string -> unit
(*e: signature Optcompile.interface *)
(*s: signature Optcompile.implementation *)
val implementation: string -> unit
(*e: signature Optcompile.implementation *)
(*s: signature Optcompile.c_file *)
val c_file: string -> unit
(*e: signature Optcompile.c_file *)

(*s: signature Optcompile.initial_env *)
val initial_env: unit -> Env.t
(*e: signature Optcompile.initial_env *)
(*s: signature Optcompile.init_path *)
val init_path: unit -> unit
(*e: signature Optcompile.init_path *)
(*e: ./driver/optcompile.mli *)
