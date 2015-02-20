(*s: ./bytecomp/symtable.mli *)
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

(* $Id: symtable.mli,v 1.8 1997/06/13 15:48:53 xleroy Exp $ *)

(* Assign locations and numbers to globals and primitives *)

open Emitcode

(*s: signature Symtable.init *)
(* Functions for batch linking *)

val init: unit -> unit
(*e: signature Symtable.init *)
(*s: signature Symtable.patch_object *)
val patch_object: string -> (reloc_info * int) list -> unit
(*e: signature Symtable.patch_object *)
(*s: signature Symtable.require_primitive *)
val require_primitive: string -> unit
(*e: signature Symtable.require_primitive *)
(*s: signature Symtable.initial_global_table *)
val initial_global_table: unit -> Obj.t array
(*e: signature Symtable.initial_global_table *)
(*s: signature Symtable.output_global_map *)
val output_global_map: out_channel -> unit
(*e: signature Symtable.output_global_map *)
(*s: signature Symtable.output_primitive_names *)
val output_primitive_names: out_channel -> unit
(*e: signature Symtable.output_primitive_names *)
(*s: signature Symtable.output_primitive_table *)
val output_primitive_table: out_channel -> unit
(*e: signature Symtable.output_primitive_table *)

(*s: signature Symtable.init_toplevel *)
(* Functions for the toplevel *)

val init_toplevel: unit -> unit
(*e: signature Symtable.init_toplevel *)
(*s: signature Symtable.update_global_table *)
val update_global_table: unit -> unit
(*e: signature Symtable.update_global_table *)
(*s: signature Symtable.get_global_value *)
val get_global_value: Ident.t -> Obj.t
(*e: signature Symtable.get_global_value *)
(*s: signature Symtable.get_global_position *)
val get_global_position: Ident.t -> int
(*e: signature Symtable.get_global_position *)

type global_map

(*s: signature Symtable.current_state *)
val current_state: unit -> global_map
(*e: signature Symtable.current_state *)
(*s: signature Symtable.restore_state *)
val restore_state: global_map -> unit
(*e: signature Symtable.restore_state *)
(*s: signature Symtable.hide_additions *)
val hide_additions: global_map -> unit
(*e: signature Symtable.hide_additions *)
(*s: signature Symtable.filter_global_map *)
val filter_global_map: (Ident.t -> bool) -> global_map -> global_map
(*e: signature Symtable.filter_global_map *)

(*s: type Symtable.error *)
(* Error report *)

type error =
    Undefined_global of string
  | Unavailable_primitive of string
(*e: type Symtable.error *)

(*s: exception Symtable.Error *)
exception Error of error
(*e: exception Symtable.Error *)

(*s: signature Symtable.report_error *)
val report_error: error -> unit
(*e: signature Symtable.report_error *)
(*e: ./bytecomp/symtable.mli *)
