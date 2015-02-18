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

(* $Id: ccomp.mli,v 1.3 1997/06/23 14:36:30 doligez Exp $ *)

(* Compiling C files and building C libraries *)

val command: string -> int
val compile_file_bytecode: string -> int
val compile_file_native: string -> int
val create_archive: string -> string list -> int
