(*s: ./driver/optcompile.mli *)
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
