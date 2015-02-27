(*s: ./parsing/parsetree.mli *)
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

(* Abstract syntax tree produced by parsing *)

open Asttypes

(* Type expressions for the core language *)

(*s: type Parsetree.core_type *)
type core_type =
  { ptyp_desc: core_type_desc;
    ptyp_loc: Location.t }
(*e: type Parsetree.core_type *)

(*s: type Parsetree.core_type_desc *)
and core_type_desc = 
  | Ptyp_var of string
  | Ptyp_arrow of core_type * core_type
  | Ptyp_tuple of core_type list
  | Ptyp_constr of Longident.t * core_type list
  (*s: [[Parsetree.core_type_desc]] cases *)
  | Ptyp_any
  (*x: [[Parsetree.core_type_desc]] cases *)
  | Ptyp_alias of core_type * string
  (*e: [[Parsetree.core_type_desc]] cases *)
(*e: type Parsetree.core_type_desc *)

(* Value expressions for the core language *)

(*s: type Parsetree.pattern *)
type pattern =
  { ppat_desc: pattern_desc;
    ppat_loc: Location.t }
(*e: type Parsetree.pattern *)

(*s: type Parsetree.pattern_desc *)
and pattern_desc =
    Ppat_any
  | Ppat_var of string
  | Ppat_alias of pattern * string

  | Ppat_constant of constant
  | Ppat_tuple of pattern list
  | Ppat_construct of Longident.t * pattern option
  | Ppat_record of (Longident.t * pattern) list

  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * core_type
(*e: type Parsetree.pattern_desc *)

(*s: type Parsetree.expression *)
type expression =
  { pexp_desc: expression_desc;
    pexp_loc: Location.t }
(*e: type Parsetree.expression *)

(*s: type Parsetree.expression_desc *)
and expression_desc =
  | Pexp_constant of Asttypes.constant

  | Pexp_tuple of expression list
  | Pexp_construct of Longident.t * expression option
  (* todo: Pexp_record_with *)
  | Pexp_record of (Longident.t * expression) list

  (* lambda calcul! abs and app *)
  | Pexp_function of (pattern * expression) list
  | Pexp_apply of expression * expression list

  | Pexp_field of expression * Longident.t
  | Pexp_setfield of expression * Longident.t * expression

  | Pexp_let of rec_flag * (pattern * expression) list * expression
  | Pexp_ident of Longident.t

  | Pexp_match of expression * (pattern * expression) list
  (* todo? not only in match? *)
  | Pexp_when of expression * expression

  | Pexp_constraint of expression * core_type

  (*s: [[Parsetree.expression_desc]] cases *)
  | Pexp_sequence of expression * expression

  | Pexp_ifthenelse of expression * expression * expression option
  | Pexp_while of expression * expression
  | Pexp_for of string * expression * expression * direction_flag * expression

  | Pexp_try of expression * (pattern * expression) list
  (*x: [[Parsetree.expression_desc]] cases *)
  | Pexp_array of expression list
  (*e: [[Parsetree.expression_desc]] cases *)
(*e: type Parsetree.expression_desc *)

(*s: type Parsetree.value_description *)
(* Value descriptions *)

type value_description =
  { pval_type: core_type;
    pval_prim: string list }
(*e: type Parsetree.value_description *)

(* Type declarations *)

(*s: type Parsetree.type_declaration *)
type type_declaration =
  { ptype_params: string list;
    ptype_kind: type_kind;
    ptype_loc: Location.t;
    (*s: [[Parsetree.type_declaration]] other fields *)
    ptype_manifest: core_type option;
    (*e: [[Parsetree.type_declaration]] other fields *)
  }
(*e: type Parsetree.type_declaration *)

(*s: type Parsetree.type_kind *)
and type_kind =
  | Ptype_variant of (string * core_type list) list
  | Ptype_record of (string * mutable_flag * core_type) list
  (*s: [[Parsetree.type_kind]] cases *)
  | Ptype_abstract
  (*e: [[Parsetree.type_kind]] cases *)
(*e: type Parsetree.type_kind *)

(*s: type Parsetree.exception_declaration *)
type exception_declaration = core_type list
(*e: type Parsetree.exception_declaration *)

(* Type expressions for the module language *)

(*s: type Parsetree.module_type *)
type module_type =
  { pmty_desc: module_type_desc;
    pmty_loc: Location.t }
(*e: type Parsetree.module_type *)

(*s: type Parsetree.module_type_desc *)
and module_type_desc =
    Pmty_ident of Longident.t
  | Pmty_signature of signature
(*e: type Parsetree.module_type_desc *)


(*s: type Parsetree.signature *)
and signature = signature_item list
(*e: type Parsetree.signature *)

(*s: type Parsetree.signature_item *)
and signature_item =
  { psig_desc: signature_item_desc;
    psig_loc: Location.t }
(*e: type Parsetree.signature_item *)

(*s: type Parsetree.signature_item_desc *)
and signature_item_desc =
    Psig_value of string * value_description
  | Psig_type of (string * type_declaration) list
  | Psig_exception of string * exception_declaration

  | Psig_module of string * module_type
  | Psig_open of Longident.t
(*e: type Parsetree.signature_item_desc *)

(* Value expressions for the module language *)

(*s: type Parsetree.module_expr *)
type module_expr =
  { pmod_desc: module_expr_desc;
    pmod_loc: Location.t }
(*e: type Parsetree.module_expr *)

(*s: type Parsetree.module_expr_desc *)
and module_expr_desc =
    Pmod_ident of Longident.t
  | Pmod_structure of structure
  | Pmod_constraint of module_expr * module_type
(*e: type Parsetree.module_expr_desc *)

(*s: type Parsetree.structure *)
and structure = structure_item list
(*e: type Parsetree.structure *)

(*s: type Parsetree.structure_item *)
and structure_item =
  { pstr_desc: structure_item_desc;
    pstr_loc: Location.t }
(*e: type Parsetree.structure_item *)

(*s: type Parsetree.structure_item_desc *)
and structure_item_desc =
    Pstr_eval of expression
  | Pstr_value of rec_flag * (pattern * expression) list
  | Pstr_primitive of string * value_description

  | Pstr_type of (string * type_declaration) list
  | Pstr_exception of string * exception_declaration

  | Pstr_module of string * module_expr
  | Pstr_open of Longident.t
(*e: type Parsetree.structure_item_desc *)

(* Toplevel phrases *)

(*s: type Parsetree.toplevel_phrase *)
type toplevel_phrase =
    Ptop_def of structure
  | Ptop_dir of string * directive_argument
(*e: type Parsetree.toplevel_phrase *)

(*s: type Parsetree.directive_argument *)
and directive_argument =
    Pdir_none
  | Pdir_string of string
  | Pdir_int of int
  | Pdir_ident of Longident.t
(*e: type Parsetree.directive_argument *)
(*e: ./parsing/parsetree.mli *)
