(*s: ./typing/includecore.mli *)
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

(* $Id$ *)

(* Inclusion checks for the core language *)

open Types
open Typedtree

(*s: exception Includecore.Dont_match *)
exception Dont_match
(*e: exception Includecore.Dont_match *)

(*s: signature Includecore.value_descriptions *)
val value_descriptions:
        Env.t -> value_description -> value_description -> module_coercion
(*e: signature Includecore.value_descriptions *)
(*s: signature Includecore.type_declarations *)
val type_declarations:
        Env.t -> Ident.t -> type_declaration -> type_declaration -> bool
(*e: signature Includecore.type_declarations *)
(*s: signature Includecore.exception_declarations *)
val exception_declarations:
        Env.t -> exception_declaration -> exception_declaration -> bool
(*e: signature Includecore.exception_declarations *)


(*e: ./typing/includecore.mli *)
