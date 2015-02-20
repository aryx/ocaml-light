(*s: ./typing/typetexp.mli *)
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

(*s: signature Typetexp.transl_simple_type *)
(* Typechecking of type expressions for the core language *)

val transl_simple_type:
        Env.t -> bool -> Parsetree.core_type -> Types.type_expr
(*e: signature Typetexp.transl_simple_type *)
(*s: signature Typetexp.transl_simple_type_delayed *)
val transl_simple_type_delayed:
        Env.t -> Parsetree.core_type -> Types.type_expr * (unit -> unit)
        (* Translate a type, but leave type variables unbound. Returns
           the type and a function that binds the type variable. *)
(*e: signature Typetexp.transl_simple_type_delayed *)
(*s: signature Typetexp.transl_type_scheme *)
val transl_type_scheme:
        Env.t -> Parsetree.core_type -> Types.type_expr
(*e: signature Typetexp.transl_type_scheme *)
(*s: signature Typetexp.reset_type_variables *)
val reset_type_variables: unit -> unit
(*e: signature Typetexp.reset_type_variables *)
(*s: signature Typetexp.enter_type_variable *)
val enter_type_variable: bool -> string -> Types.type_expr
(*e: signature Typetexp.enter_type_variable *)
(*s: signature Typetexp.type_variable *)
val type_variable : Location.t -> string -> Types.type_expr
(*e: signature Typetexp.type_variable *)

(*s: exception Typetexp.Already_bound *)
exception Already_bound
(*e: exception Typetexp.Already_bound *)

(*s: type Typetexp.error *)
type error =
    Unbound_type_variable of string
  | Unbound_type_constructor of Longident.t
  | Type_arity_mismatch of Longident.t * int * int
  | Bound_type_variable of string
  | Type_mismatch of (Types.type_expr * Types.type_expr) list
  | Alias_type_mismatch of (Types.type_expr * Types.type_expr) list
(*e: type Typetexp.error *)

(*s: exception Typetexp.Error *)
exception Error of Location.t * error
(*e: exception Typetexp.Error *)

(*s: signature Typetexp.report_error *)
val report_error: error -> unit
(*e: signature Typetexp.report_error *)
(*e: ./typing/typetexp.mli *)
