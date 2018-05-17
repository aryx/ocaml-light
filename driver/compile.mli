(*s: ./driver/compile.mli *)
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

(* Compile a .ml or .mli file *)

(*s: signature [[Compile.interface]] *)
val interface: string -> unit
(*e: signature [[Compile.interface]] *)
(*s: signature [[Compile.implementation]] *)
val implementation: string -> unit
(*e: signature [[Compile.implementation]] *)
(*s: signature [[Compile.c_file]] *)
val c_file: string -> unit
(*e: signature [[Compile.c_file]] *)

(*s: signature [[Compile.initial_env]] *)
val initial_env: unit -> Env.t
(*e: signature [[Compile.initial_env]] *)
(*s: signature [[Compile.init_path]] *)
val init_path: unit -> unit
(*e: signature [[Compile.init_path]] *)
(*e: ./driver/compile.mli *)
