(*s: ./utils/ccomp.mli *)
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

(* Compiling C files and building C libraries *)

(*s: signature Ccomp.command *)
val command: string -> int
(*e: signature Ccomp.command *)
(*s: signature Ccomp.compile_file_bytecode *)
val compile_file_bytecode: string -> int
(*e: signature Ccomp.compile_file_bytecode *)
(*s: signature Ccomp.compile_file_native *)
val compile_file_native: string -> int
(*e: signature Ccomp.compile_file_native *)
(*s: signature Ccomp.create_archive *)
val create_archive: string -> string list -> int
(*e: signature Ccomp.create_archive *)
(*e: ./utils/ccomp.mli *)
