(*s: typing/typecore.mli *)
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

(* Type inference for the core language *)

open Asttypes
open Types

(*s: signature [[Typecore.type_binding]] *)
val type_binding:
        Env.t -> rec_flag ->
          (Parsetree.pattern * Parsetree.expression) list -> 
            (Typedtree.pattern * Typedtree.expression) list * Env.t
(*e: signature [[Typecore.type_binding]] *)

(*s: signature [[Typecore.type_expression]] *)
val type_expression:
        Env.t -> Parsetree.expression -> Typedtree.expression
(*e: signature [[Typecore.type_expression]] *)
        
(*s: type [[Typecore.error]] *)
type error =
    Unbound_value       of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label       of Longident.t

  | Constructor_arity_mismatch of Longident.t * int * int
  | Label_mismatch of Longident.t * type_expr * type_expr
  | Pattern_type_clash of type_expr * type_expr

  | Multiply_bound_variable
  | Orpat_not_closed
  | Expr_type_clash of type_expr * type_expr
  | Apply_non_function of type_expr

  | Label_multiply_defined of Longident.t
  | Label_missing
  | Label_not_mutable of Longident.t

  | Bad_format of string

  | Too_many_arguments
(*e: type [[Typecore.error]] *)

(*s: exception [[Typecore.Error]] *)
exception Error of Location.t * error
(*e: exception [[Typecore.Error]] *)

(*s: signature [[Typecore.report_error]] *)
val report_error: error -> unit
(*e: signature [[Typecore.report_error]] *)
(*e: typing/typecore.mli *)
