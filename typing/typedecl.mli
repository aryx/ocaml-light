(*s: ./typing/typedecl.mli *)
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

(* Typing of type definitions and primitive definitions *)

open Types
open Typedtree

(*s: signature Typedecl.transl_type_decl *)
val transl_type_decl:
        Env.t -> (string * Parsetree.type_declaration) list ->
                         (Ident.t * Types.type_declaration) list * Env.t
(*e: signature Typedecl.transl_type_decl *)
(*s: signature Typedecl.transl_exception *)
val transl_exception:
        Env.t -> Parsetree.exception_declaration -> Types.exception_declaration
(*e: signature Typedecl.transl_exception *)

(*s: signature Typedecl.transl_value_decl *)
val transl_value_decl:
        Env.t -> Parsetree.value_description -> Types.value_description
(*e: signature Typedecl.transl_value_decl *)

(*s: signature Typedecl.transl_with_constraint *)
val transl_with_constraint:
        Env.t -> Parsetree.type_declaration -> type_declaration
(*e: signature Typedecl.transl_with_constraint *)
    
(*s: type Typedecl.error *)
type error =
    Repeated_parameter
  | Duplicate_constructor of string
  | Too_many_constructors
  | Duplicate_label of string
  | Recursive_abbrev of string
  | Definition_mismatch of type_expr
(*e: type Typedecl.error *)

(*s: exception Typedecl.Error *)
exception Error of Location.t * error
(*e: exception Typedecl.Error *)

(*s: signature Typedecl.report_error *)
val report_error: error -> unit
(*e: signature Typedecl.report_error *)
(*e: ./typing/typedecl.mli *)
