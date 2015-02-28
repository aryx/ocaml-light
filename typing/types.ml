(*s: ./typing/types.ml *)
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

open Asttypes

(* Type expressions for the core language *)

(*s: type Types.type_expr *)
type type_expr =
    Tvar of type_variable

  | Tarrow of type_expr * type_expr
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list
(*e: type Types.type_expr *)

(*s: type Types.type_variable *)
and type_variable =
    { mutable tvar_level: int;
      mutable tvar_link: type_expr option }
(*e: type Types.type_variable *)

(*s: type Types.value_description *)
(* Value descriptions *)

type value_description =
  { val_type: type_expr;                       (* Type of the val *)
    val_prim: Primitive.description option }   (* Is this a primitive? *)
(*e: type Types.value_description *)

(* Constructor descriptions *)

(*s: type Types.constructor_description *)
type constructor_description =
  { cstr_res: type_expr;                (* Type of the result *)
    cstr_args: type_expr list;          (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)

    (*s: [[Types.constructor_description]] other fields *)
    cstr_tag: constructor_tag;          (* Tag for heap blocks *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int;               (* Number of non-const constructors *)
    (*e: [[Types.constructor_description]] other fields *)
   }
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

    (*s: [[Types.label_description]] other fields *)
    lbl_pos: int;                       (* Position in block *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation;  (* Representation for this record *)
    (*e: [[Types.label_description]] other fields *)
  }
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
    type_arity: int; (* List.length td.type_params *)
    type_kind: type_kind;
    (*s: [[Types.type_declaration]] other fields *)
    type_manifest: type_expr option 
    (*e: [[Types.type_declaration]] other fields *)
  }
(*e: type Types.type_declaration *)

(*s: type Types.type_kind *)
and type_kind =
  | Type_variant of (string * type_expr list) list
  | Type_record of (string * mutable_flag * type_expr) list
  (*s: [[Types.type_kind]] cases *)
  | Type_abstract
  (*e: [[Types.type_kind]] cases *)
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
(*e: type Types.signature_item *)
(*e: ./typing/types.ml *)
