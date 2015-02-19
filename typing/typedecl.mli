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

(* $Id: typedecl.mli,v 1.11 1997/05/13 14:07:00 xleroy Exp $ *)

(* Typing of type definitions and primitive definitions *)

open Types

val transl_type_decl:
        Env.t -> (string * Parsetree.type_declaration) list ->
                                  (Ident.t * type_declaration) list * Env.t
val transl_exception:
        Env.t -> Parsetree.exception_declaration -> exception_declaration

val transl_value_decl:
        Env.t -> Parsetree.value_description -> value_description

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

exception Error of Location.t * error

val report_error: error -> unit
