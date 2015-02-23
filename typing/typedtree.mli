(*s: ./typing/typedtree.mli *)
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

(* Abstract syntax tree after typing *)

open Asttypes
open Types

(* Value expressions for the core language *)

(*s: type Typedtree.pattern *)
type pattern =
  { pat_desc: pattern_desc;
    pat_loc: Location.t;
    pat_type: type_expr }
(*e: type Typedtree.pattern *)

(*s: type Typedtree.pattern_desc *)
and pattern_desc =
    Tpat_any
  | Tpat_var of Ident.t
  | Tpat_alias of pattern * Ident.t
  | Tpat_constant of constant
  | Tpat_tuple of pattern list
  | Tpat_construct of constructor_description * pattern list
  | Tpat_record of (label_description * pattern) list
  | Tpat_or of pattern * pattern
(*e: type Typedtree.pattern_desc *)

(*s: type Typedtree.expression *)
type expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_type: type_expr; }
(*e: type Typedtree.expression *)

(*s: type Typedtree.expression_desc *)
and expression_desc =
    Texp_ident of Path.t * value_description
  | Texp_constant of constant
  | Texp_let of rec_flag * (pattern * expression) list * expression
  | Texp_function of (pattern * expression) list
  | Texp_apply of expression * expression list
  | Texp_match of expression * (pattern * expression) list
  | Texp_try of expression * (pattern * expression) list
  | Texp_tuple of expression list
  | Texp_construct of constructor_description * expression list
  | Texp_record of (label_description * expression) list
  | Texp_field of expression * label_description
  | Texp_setfield of expression * label_description * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t * expression * expression * direction_flag * expression
  | Texp_when of expression * expression
(*e: type Typedtree.expression_desc *)

(* Value expressions for the module language *)

(*s: type Typedtree.module_expr *)
type module_expr =
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: module_type; }
(*e: type Typedtree.module_expr *)

(*s: type Typedtree.module_expr_desc *)
and module_expr_desc =
    Tmod_ident of Path.t
  | Tmod_structure of structure
  | Tmod_constraint of module_expr * module_type * module_coercion
(*e: type Typedtree.module_expr_desc *)

(*s: type Typedtree.structure *)
and structure = structure_item list
(*e: type Typedtree.structure *)

(*s: type Typedtree.structure_item *)
and structure_item =
    Tstr_eval of expression
  | Tstr_value of rec_flag * (pattern * expression) list
  | Tstr_primitive of Ident.t * value_description
  | Tstr_type of (Ident.t * type_declaration) list
  | Tstr_exception of Ident.t * exception_declaration
  | Tstr_module of Ident.t * module_expr
  | Tstr_open of Path.t
(*e: type Typedtree.structure_item *)

(*s: type Typedtree.module_coercion *)
and module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_primitive of Primitive.description
(*e: type Typedtree.module_coercion *)

(*s: signature Typedtree.pat_bound_idents *)
(* Auxiliary functions over the a.s.t. *)

val pat_bound_idents: pattern -> Ident.t list
(*e: signature Typedtree.pat_bound_idents *)
(*s: signature Typedtree.let_bound_idents *)
val let_bound_idents: (pattern * expression) list -> Ident.t list
(*e: signature Typedtree.let_bound_idents *)
(*s: signature Typedtree.rev_let_bound_idents *)
val rev_let_bound_idents: (pattern * expression) list -> Ident.t list
(*e: signature Typedtree.rev_let_bound_idents *)
(*e: ./typing/typedtree.mli *)
