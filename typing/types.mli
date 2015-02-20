(*s: ./typing/types.mli *)
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

(* Representation of types and declarations *)

open Asttypes

(* Type expressions for the core language *)

(*s: type Types.type_expr *)
type type_expr =
  { mutable desc: type_desc; 
    mutable level: int }
(*e: type Types.type_expr *)

(*s: type Types.type_desc *)
and type_desc =
    Tvar
  | Tarrow of type_expr * type_expr
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tnil
  | Tlink of type_expr
(*e: type Types.type_desc *)

(*s: type Types.abbrev_memo *)
and abbrev_memo =
    Mnil
  | Mcons of Path.t * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref
(*e: type Types.abbrev_memo *)

(* Value descriptions *)

(*s: type Types.value_description *)
type value_description =
  { val_type: type_expr;                (* Type of the value *)
    val_kind: value_kind }
(*e: type Types.value_description *)

(*s: type Types.value_kind *)
and value_kind =
    Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)
(*e: type Types.value_kind *)

(* Constructor descriptions *)

(*s: type Types.constructor_description *)
type constructor_description =
  { cstr_res: type_expr;                (* Type of the result *)
    cstr_args: type_expr list;          (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: constructor_tag;          (* Tag for heap blocks *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int }               (* Number of non-const constructors *)
(*e: type Types.constructor_description *)

(*s: type Types.constructor_tag *)
and constructor_tag =
    Cstr_constant of int                (* Constant constructor (an int) *)
  | Cstr_block of int                   (* Regular constructor (a block) *)
  | Cstr_exception of Path.t            (* Exception constructor *)
(*e: type Types.constructor_tag *)

(* Record label descriptions *)

(*s: type Types.label_description *)
type label_description =
  { lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutable_flag;              (* Is this a mutable field? *)
    lbl_pos: int;                       (* Position in block *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation } (* Representation for this record *)
(*e: type Types.label_description *)

(*s: type Types.record_representation *)
and record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)
(*e: type Types.record_representation *)

(* Type definitions *)

(*s: type Types.type_declaration *)
type type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_kind;
    type_manifest: type_expr option }
(*e: type Types.type_declaration *)

(*s: type Types.type_kind *)
and type_kind =
    Type_abstract
  | Type_variant of (string * type_expr list) list
  | Type_record of (string * mutable_flag * type_expr) list
(*e: type Types.type_kind *)

(*s: type Types.exception_declaration *)
type exception_declaration = type_expr list
(*e: type Types.exception_declaration *)

(* Type expressions for the module language *)

(*s: type Types.module_type *)
type module_type =
    Tmty_ident of Path.t
  | Tmty_signature of signature
(*e: type Types.module_type *)

(*s: type Types.signature *)
and signature = signature_item list
(*e: type Types.signature *)

(*s: type Types.signature_item *)
and signature_item =
    Tsig_value of Ident.t * value_description
  | Tsig_type of Ident.t * type_declaration
  | Tsig_exception of Ident.t * exception_declaration
  | Tsig_module of Ident.t * module_type
  | Tsig_modtype of Ident.t * modtype_declaration
(*e: type Types.signature_item *)

(*s: type Types.modtype_declaration *)
and modtype_declaration =
    Tmodtype_abstract
  | Tmodtype_manifest of module_type
(*e: type Types.modtype_declaration *)
(*e: ./typing/types.mli *)
