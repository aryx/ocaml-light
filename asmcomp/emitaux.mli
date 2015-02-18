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

(* $Id: emitaux.mli,v 1.7 1997/03/04 10:19:49 xleroy Exp $ *)

(* Common functions for emitting assembly code *)

val output_channel: out_channel ref
val emit_string: string -> unit
val emit_int: int -> unit
val emit_nativeint: Nativeint.t -> unit
val emit_symbol: char -> string -> unit
val emit_printf: ('a, out_channel, unit) format -> 'a
val emit_char: char -> unit
val emit_string_literal: string -> unit
val emit_string_directive: string -> string -> unit
val emit_bytes_directive: string -> string -> unit
