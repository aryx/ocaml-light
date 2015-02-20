(*s: ./typing/typedecl.mli *)
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

(* Typing of type definitions and primitive definitions *)

open Types

(*s: signature Typedecl.transl_type_decl *)
val transl_type_decl:
        Env.t -> (string * Parsetree.type_declaration) list ->
                                  (Ident.t * type_declaration) list * Env.t
(*e: signature Typedecl.transl_type_decl *)
(*s: signature Typedecl.transl_exception *)
val transl_exception:
        Env.t -> Parsetree.exception_declaration -> exception_declaration
(*e: signature Typedecl.transl_exception *)

(*s: signature Typedecl.transl_value_decl *)
val transl_value_decl:
        Env.t -> Parsetree.value_description -> value_description
(*e: signature Typedecl.transl_value_decl *)

(*s: type Typedecl.error *)
type error =
    Repeated_parameter
  | Duplicate_constructor of string
  | Too_many_constructors
  | Duplicate_label of string
  | Recursive_abbrev of string
  | Definition_mismatch of type_expr
  | Unconsistent_constraint
  | Type_clash of (type_expr * type_expr) list
  | Null_arity_external
(*e: type Typedecl.error *)

(*s: exception Typedecl.Error *)
exception Error of Location.t * error
(*e: exception Typedecl.Error *)

(*s: signature Typedecl.report_error *)
val report_error: error -> unit
(*e: signature Typedecl.report_error *)
(*e: ./typing/typedecl.mli *)
