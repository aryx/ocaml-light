(*s: typing/printtyp.mli *)
(*s: copyright header0 *)
(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header0 *)

(* Printing functions *)
open Types
open Typedtree

(*s: signature [[Printtyp.longident]] *)
val longident: Longident.t -> unit
(*e: signature [[Printtyp.longident]] *)
(*s: signature [[Printtyp.ident]] *)
val ident: Ident.t -> unit
(*e: signature [[Printtyp.ident]] *)
(*s: signature [[Printtyp.path]] *)
val path: Path.t -> unit
(*e: signature [[Printtyp.path]] *)
(*s: signature [[Printtyp.reset_var_names]] *)
val reset_var_names: unit -> unit
(*e: signature [[Printtyp.reset_var_names]] *)
(*s: signature [[Printtyp.type_expr]] *)
val type_expr: type_expr -> unit
(*e: signature [[Printtyp.type_expr]] *)
(*s: signature [[Printtyp.type_scheme]] *)
val type_scheme: type_expr -> unit
(*e: signature [[Printtyp.type_scheme]] *)
(*s: signature [[Printtyp.value_description]] *)
val value_description: Ident.t -> value_description -> unit
(*e: signature [[Printtyp.value_description]] *)
(*s: signature [[Printtyp.type_declaration]] *)
val type_declaration: Ident.t -> type_declaration -> unit
(*e: signature [[Printtyp.type_declaration]] *)
(*s: signature [[Printtyp.exception_declaration]] *)
val exception_declaration: Ident.t -> exception_declaration -> unit
(*e: signature [[Printtyp.exception_declaration]] *)
(*s: signature [[Printtyp.modtype]] *)
val modtype: module_type -> unit
(*e: signature [[Printtyp.modtype]] *)
(*s: signature [[Printtyp.signature]] *)
val signature: signature -> unit
(*e: signature [[Printtyp.signature]] *)
(*s: signature [[Printtyp.signature_item]] *)
val signature_item: signature_item -> unit
(*e: signature [[Printtyp.signature_item]] *)
(*e: typing/printtyp.mli *)
