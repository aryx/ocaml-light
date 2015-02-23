(*s: ./typing/typedtree.ml *)
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

(* Abstract syntax tree after typing *)

open Misc
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

(* Auxiliary functions over the a.s.t. *)

(*s: constant Typedtree.idents *)
(* List the identifiers bound by a pattern or a let *)

let idents = ref([]: Ident.t list)
(*e: constant Typedtree.idents *)

(*s: function Typedtree.bound_idents *)
let rec bound_idents pat =
  match pat.pat_desc with
    Tpat_any -> ()
  | Tpat_var id -> idents := id :: !idents
  | Tpat_alias(p, id) -> bound_idents p; idents := id :: !idents
  | Tpat_constant cst -> ()
  | Tpat_tuple patl -> List.iter bound_idents patl
  | Tpat_construct(cstr, patl) -> List.iter bound_idents patl
  | Tpat_record lbl_pat_list ->
      List.iter (fun (lbl, pat) -> bound_idents pat) lbl_pat_list
  | Tpat_or(p1, p2) -> bound_idents p1; bound_idents p2
(*e: function Typedtree.bound_idents *)

(*s: function Typedtree.pat_bound_idents *)
let pat_bound_idents pat =
  idents := []; bound_idents pat; let res = !idents in idents := []; res
(*e: function Typedtree.pat_bound_idents *)

(*s: function Typedtree.rev_let_bound_idents *)
let rev_let_bound_idents pat_expr_list =
  idents := [];
  List.iter (fun (pat, expr) -> bound_idents pat) pat_expr_list;
  let res = !idents in idents := []; res
(*e: function Typedtree.rev_let_bound_idents *)

(*s: function Typedtree.let_bound_idents *)
let let_bound_idents pat_expr_list =
  List.rev(rev_let_bound_idents pat_expr_list)
(*e: function Typedtree.let_bound_idents *)

      
(*e: ./typing/typedtree.ml *)
