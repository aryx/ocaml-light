(*s: ./typing/typecore.mli *)
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

(* Type inference for the core language *)

open Asttypes
open Types

(*s: signature Typecore.type_binding *)
val type_binding:
        Env.t -> rec_flag ->
          (Parsetree.pattern * Parsetree.expression) list -> 
          (Typedtree.pattern * Typedtree.expression) list * Env.t
(*e: signature Typecore.type_binding *)
(*s: signature Typecore.type_expression *)
val type_expression:
        Env.t -> Parsetree.expression -> Typedtree.expression
(*e: signature Typecore.type_expression *)
(*s: signature Typecore.type_pattern_list *)
val type_pattern_list:
        Env.t -> Parsetree.pattern list -> Typedtree.pattern list * Env.t
(*e: signature Typecore.type_pattern_list *)
(*s: signature Typecore.type_expect *)
val type_expect:
        Env.t -> Parsetree.expression -> type_expr -> Typedtree.expression
(*e: signature Typecore.type_expect *)
(*s: signature Typecore.type_exp *)
val type_exp:
        Env.t -> Parsetree.expression -> Typedtree.expression
(*e: signature Typecore.type_exp *)

(*s: type Typecore.error *)
type error =
    Unbound_value of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label of Longident.t
  | Constructor_arity_mismatch of Longident.t * int * int
  | Label_mismatch of Longident.t * (type_expr * type_expr) list
  | Pattern_type_clash of (type_expr * type_expr) list
  | Multiply_bound_variable
  | Orpat_not_closed
  | Expr_type_clash of (type_expr * type_expr) list
  | Apply_non_function of type_expr
  | Label_multiply_defined of Longident.t
  | Label_missing
  | Label_not_mutable of Longident.t
  | Bad_format of string
  | Not_subtype of (type_expr * type_expr) list * (type_expr * type_expr) list
  | Coercion_failure of type_expr * type_expr * (type_expr * type_expr) list
  | Too_many_arguments
(*e: type Typecore.error *)

(*s: exception Typecore.Error *)
exception Error of Location.t * error
(*e: exception Typecore.Error *)

(*s: signature Typecore.report_error *)
val report_error: error -> unit
(*e: signature Typecore.report_error *)
(*e: ./typing/typecore.mli *)
