(*s: ./typing/subst.mli *)
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

(* Substitutions *)

open Types

type t

(*s: signature [[Subst.identity]] *)
val identity: t
(*e: signature [[Subst.identity]] *)

(*s: signature [[Subst.add_type]] *)
val add_type: Ident.t -> Path.t -> t -> t
(*e: signature [[Subst.add_type]] *)
(*s: signature [[Subst.add_module]] *)
val add_module: Ident.t -> Path.t -> t -> t
(*e: signature [[Subst.add_module]] *)
(*s: signature [[Subst.add_modtype]] *)
val add_modtype: Ident.t -> module_type -> t -> t
(*e: signature [[Subst.add_modtype]] *)

(*s: signature [[Subst.type_expr]] *)
val type_expr: t -> type_expr -> type_expr
(*e: signature [[Subst.type_expr]] *)
(*s: signature [[Subst.value_description]] *)
val value_description: t -> value_description -> value_description
(*e: signature [[Subst.value_description]] *)
(*s: signature [[Subst.type_declaration]] *)
val type_declaration: t -> type_declaration -> type_declaration
(*e: signature [[Subst.type_declaration]] *)
(*s: signature [[Subst.exception_declaration]] *)
val exception_declaration: t -> exception_declaration -> exception_declaration
(*e: signature [[Subst.exception_declaration]] *)
(*s: signature [[Subst.modtype]] *)
val modtype: t -> module_type -> module_type
(*e: signature [[Subst.modtype]] *)
(*s: signature [[Subst.signature]] *)
val signature: t -> signature -> signature
(*e: signature [[Subst.signature]] *)

(*e: ./typing/subst.mli *)
