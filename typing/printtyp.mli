(*s: ./typing/printtyp.mli *)
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

(* Printing functions *)

open Types

(*s: signature Printtyp.longident *)
val longident: Longident.t -> unit
(*e: signature Printtyp.longident *)
(*s: signature Printtyp.ident *)
val ident: Ident.t -> unit
(*e: signature Printtyp.ident *)
(*s: signature Printtyp.path *)
val path: Path.t -> unit
(*e: signature Printtyp.path *)
(*s: signature Printtyp.reset *)
val reset: unit -> unit
(*e: signature Printtyp.reset *)
(*s: signature Printtyp.mark_loops *)
val mark_loops: type_expr -> unit
(*e: signature Printtyp.mark_loops *)
(*s: signature Printtyp.type_expr *)
val type_expr: type_expr -> unit
(*e: signature Printtyp.type_expr *)
(*s: signature Printtyp.type_scheme *)
val type_scheme: type_expr -> unit
(*e: signature Printtyp.type_scheme *)
(*s: signature Printtyp.value_description *)
val value_description: Ident.t -> value_description -> unit
(*e: signature Printtyp.value_description *)
(*s: signature Printtyp.type_declaration *)
val type_declaration: Ident.t -> type_declaration -> unit
(*e: signature Printtyp.type_declaration *)
(*s: signature Printtyp.exception_declaration *)
val exception_declaration: Ident.t -> exception_declaration -> unit
(*e: signature Printtyp.exception_declaration *)
(*s: signature Printtyp.modtype *)
val modtype: module_type -> unit
(*e: signature Printtyp.modtype *)
(*s: signature Printtyp.signature *)
val signature: signature -> unit
(*e: signature Printtyp.signature *)
(*s: signature Printtyp.signature_body *)
val signature_body: bool -> signature -> unit
(*e: signature Printtyp.signature_body *)
(*s: signature Printtyp.modtype_declaration *)
val modtype_declaration: Ident.t -> modtype_declaration -> unit
(*e: signature Printtyp.modtype_declaration *)
(*s: signature Printtyp.type_expansion *)
val type_expansion: type_expr -> type_expr -> unit
(*e: signature Printtyp.type_expansion *)
(*s: signature Printtyp.trace *)
val trace: bool -> (unit -> unit) -> (type_expr * type_expr) list -> unit
(*e: signature Printtyp.trace *)
(*s: signature Printtyp.unification_error *)
val unification_error:
        (type_expr * type_expr) list -> (unit -> unit) -> (unit -> unit) ->
        unit
(*e: signature Printtyp.unification_error *)
(*e: ./typing/printtyp.mli *)
