(*s: asmcomp/emitaux.mli *)
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

(*s: signature [[Emitaux.output_channel]] *)
(* Common functions for emitting assembly code *)

val output_channel: out_channel ref
(*e: signature [[Emitaux.output_channel]] *)
(*s: signature [[Emitaux.emit_string]] *)
val emit_string: string -> unit
(*e: signature [[Emitaux.emit_string]] *)
(*s: signature [[Emitaux.emit_int]] *)
val emit_int: int -> unit
(*e: signature [[Emitaux.emit_int]] *)
(*s: signature [[Emitaux.emit_nativeint]] *)
val emit_nativeint: Nativeint.t -> unit
(*e: signature [[Emitaux.emit_nativeint]] *)
(*s: signature [[Emitaux.emit_symbol]] *)
val emit_symbol: char -> string -> unit
(*e: signature [[Emitaux.emit_symbol]] *)
(*s: signature [[Emitaux.emit_printf]] *)
val emit_printf: ('a, out_channel, unit) format -> 'a
(*e: signature [[Emitaux.emit_printf]] *)
(*s: signature [[Emitaux.emit_char]] *)
val emit_char: char -> unit
(*e: signature [[Emitaux.emit_char]] *)
(*s: signature [[Emitaux.emit_string_literal]] *)
val emit_string_literal: string -> unit
(*e: signature [[Emitaux.emit_string_literal]] *)
(*s: signature [[Emitaux.emit_string_directive]] *)
val emit_string_directive: string -> string -> unit
(*e: signature [[Emitaux.emit_string_directive]] *)
(*s: signature [[Emitaux.emit_bytes_directive]] *)
val emit_bytes_directive: string -> string -> unit
(*e: signature [[Emitaux.emit_bytes_directive]] *)
(*e: asmcomp/emitaux.mli *)
