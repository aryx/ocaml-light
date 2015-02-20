(*s: ./typing/subst.mli *)
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

(* $Id: subst.mli,v 1.7 1997/03/24 20:12:33 vouillon Exp $ *)

(* Substitutions *)

open Types

type t

(*s: signature Subst.identity *)
(*
   Substitutions are used to translate a type from one context to
   another.  This requires substituing paths for identifiers, and
   possibly also lowering the level of non-generic variables so that
   it be inferior to the maximum level of the new context.

   Substitutions can also be used to create a "clean" copy of a type.
   Indeed, non-variable node of a type are duplicated, with their
   levels set to generic level.  That way, the resulting type is
   well-formed (decreasing levels), even if the original one was not.
*)

val identity: t
(*e: signature Subst.identity *)

(*s: signature Subst.add_type *)
val add_type: Ident.t -> Path.t -> t -> t
(*e: signature Subst.add_type *)
(*s: signature Subst.add_module *)
val add_module: Ident.t -> Path.t -> t -> t
(*e: signature Subst.add_module *)
(*s: signature Subst.add_modtype *)
val add_modtype: Ident.t -> module_type -> t -> t
(*e: signature Subst.add_modtype *)

(*s: signature Subst.type_expr *)
val type_expr: t -> type_expr -> type_expr
(*e: signature Subst.type_expr *)
(*s: signature Subst.value_description *)
val value_description: t -> value_description -> value_description
(*e: signature Subst.value_description *)
(*s: signature Subst.type_declaration *)
val type_declaration: t -> type_declaration -> type_declaration
(*e: signature Subst.type_declaration *)
(*s: signature Subst.exception_declaration *)
val exception_declaration:
        t -> exception_declaration -> exception_declaration
(*e: signature Subst.exception_declaration *)
(*s: signature Subst.modtype *)
val modtype: t -> module_type -> module_type
(*e: signature Subst.modtype *)
(*s: signature Subst.signature *)
val signature: t -> signature -> signature
(*e: signature Subst.signature *)
(*s: signature Subst.modtype_declaration *)
val modtype_declaration: t -> modtype_declaration -> modtype_declaration
(*e: signature Subst.modtype_declaration *)
(*e: ./typing/subst.mli *)
