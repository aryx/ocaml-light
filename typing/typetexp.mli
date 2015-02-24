(*s: ./typing/typetexp.mli *)
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

(*s: signature Typetexp.transl_simple_type *)
(* Typechecking of type expressions for the core language *)

val transl_simple_type:
        Env.t -> bool -> Parsetree.core_type -> Types.type_expr
(*e: signature Typetexp.transl_simple_type *)
(*s: signature Typetexp.transl_type_scheme *)
val transl_type_scheme:
        Env.t -> Parsetree.core_type -> Types.type_expr
(*e: signature Typetexp.transl_type_scheme *)
(*s: signature Typetexp.reset_type_variables *)
val reset_type_variables: unit -> unit
(*e: signature Typetexp.reset_type_variables *)
(*s: signature Typetexp.enter_type_variable *)
val enter_type_variable: string -> Types.type_expr
(*e: signature Typetexp.enter_type_variable *)

(*s: exception Typetexp.Already_bound *)
exception Already_bound
(*e: exception Typetexp.Already_bound *)

(*s: type Typetexp.error *)
type error =
    Unbound_type_variable of string
  | Unbound_type_constructor of Longident.t
  | Type_arity_mismatch of Longident.t * int * int
(*e: type Typetexp.error *)

(*s: exception Typetexp.Error *)
exception Error of Location.t * error
(*e: exception Typetexp.Error *)

(*s: signature Typetexp.report_error *)
val report_error: error -> unit
(*e: signature Typetexp.report_error *)
(*e: ./typing/typetexp.mli *)
