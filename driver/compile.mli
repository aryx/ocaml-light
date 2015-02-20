(*s: ./driver/compile.mli *)
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

(* $Id: compile.mli,v 1.5 1996/04/30 14:45:54 xleroy Exp $ *)

(*s: signature Compile.interface *)
(* Compile a .ml or .mli file *)

val interface: string -> unit
(*e: signature Compile.interface *)
(*s: signature Compile.implementation *)
val implementation: string -> unit
(*e: signature Compile.implementation *)
(*s: signature Compile.c_file *)
val c_file: string -> unit
(*e: signature Compile.c_file *)

(*s: signature Compile.initial_env *)
val initial_env: unit -> Env.t
(*e: signature Compile.initial_env *)
(*s: signature Compile.init_path *)
val init_path: unit -> unit
(*e: signature Compile.init_path *)
(*e: ./driver/compile.mli *)
