(*s: ./typing/predef.ml *)
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

(* Predefined type constructors (with special typing rules in typecore) *)

open Path
open Types


(*s: constants Predef.ident_xxx *)
let ident_int = Ident.create "int"
and ident_char = Ident.create "char"
and ident_string = Ident.create "string"
and ident_float = Ident.create "float"
and ident_bool = Ident.create "bool"
and ident_unit = Ident.create "unit"

and ident_exn = Ident.create "exn"

and ident_array = Ident.create "array"
and ident_list = Ident.create "list"
(*x: constants Predef.ident_xxx *)
and ident_format = Ident.create "format"
(*e: constants Predef.ident_xxx *)
(*s: constants Predef.path_xxx *)
let path_int = Pident ident_int
and path_char = Pident ident_char
and path_string = Pident ident_string
and path_float = Pident ident_float
and path_bool = Pident ident_bool
and path_unit = Pident ident_unit

and path_list = Pident ident_list
(*x: constants Predef.path_xxx *)
and path_exn = Pident ident_exn
(*x: constants Predef.path_xxx *)
and path_format = Pident ident_format
(*x: constants Predef.path_xxx *)
and path_array = Pident ident_array
(*e: constants Predef.path_xxx *)

(*s: constants Predef.type_xxx *)
let type_int     = Tconstr(path_int, [])
and type_char    = Tconstr(path_char, [])
and type_string  = Tconstr(path_string, [])
and type_float   = Tconstr(path_float, [])
and type_bool    = Tconstr(path_bool, [])
and type_unit    = Tconstr(path_unit, [])

and type_list t = Tconstr(path_list, [t])
(*x: constants Predef.type_xxx *)
and type_exn = Tconstr(path_exn, [])
(*x: constants Predef.type_xxx *)
and type_array t = Tconstr(path_array, [t])
(*e: constants Predef.type_xxx *)


(*s: constants Predef.ident_exn_xxx *)
let ident_match_failure    = Ident.create "Match_failure"
and ident_out_of_memory    = Ident.create "Out_of_memory"
and ident_invalid_argument = Ident.create "Invalid_argument"
and ident_failure          = Ident.create "Failure"
and ident_not_found        = Ident.create "Not_found"
and ident_sys_error        = Ident.create "Sys_error"
and ident_end_of_file      = Ident.create "End_of_file"
and ident_division_by_zero = Ident.create "Division_by_zero"
and ident_stack_overflow   = Ident.create "Stack_overflow"
(*e: constants Predef.ident_exn_xxx *)

(*s: constant Predef.path_match_failure *)
let path_match_failure = Pident ident_match_failure
(*e: constant Predef.path_match_failure *)

(*s: function Predef.build_initial_env *)
let build_initial_env add_type add_exception empty_env =
  let newvar() =
    (* Cannot call the real newvar from ctype here
       because ctype imports predef via env *)
    Tvar{
      tvar_link = None; 
      (*s: [[Predef.build_initial_env()]] set other fields in local newvar *)
      tvar_level = -1 (*generic_level*); 
      (*e: [[Predef.build_initial_env()]] set other fields in local newvar *)
    } 
  in

  (*s: [[Predef.build_initial_env()]] decls *)
  let decl_bool =
    {type_params = [];
     type_arity = 0;
     type_kind = Type_variant["false",[]; "true",[]];
     type_manifest = None}
  in
  let decl_unit =
    {type_params = []; 
     type_arity = 0;
     type_kind = Type_variant["()",[]];
     type_manifest = None}
  in
  (*x: [[Predef.build_initial_env()]] decls *)
  let decl_list =
    let tvar = newvar() in
    {type_params = [tvar];
     type_arity = 1;
     type_kind = Type_variant["[]", []; "::", [tvar; type_list tvar]];
     type_manifest = None}
  in
  (*x: [[Predef.build_initial_env()]] decls *)
  let decl_abstr =
    {type_params = [];
     type_arity = 0;
     type_kind = Type_abstract;
     type_manifest = None}
  in
  (*x: [[Predef.build_initial_env()]] decls *)
  let decl_exn =
    {type_params = [];
     type_arity = 0;
     type_kind = Type_variant [];
     type_manifest = None}
  in
  (*x: [[Predef.build_initial_env()]] decls *)
  let decl_format =
    {type_params = [newvar(); newvar(); newvar()];
     type_arity = 3;
     type_kind = Type_abstract;
     type_manifest = None} 
  in
  (*x: [[Predef.build_initial_env()]] decls *)
  let decl_array =
    let tvar = newvar() in
    {type_params = [tvar];
     type_arity = 1;
     type_kind = Type_abstract;
     type_manifest = None}
  in
  (*e: [[Predef.build_initial_env()]] decls *)
  (*s: [[Predef.build_initial_env()]] adding exceptions *)
  add_exception ident_match_failure [Ttuple[type_string; type_int; type_int]] (
  add_exception ident_out_of_memory [] (
  add_exception ident_stack_overflow [] (
  add_exception ident_invalid_argument [type_string] (
  add_exception ident_failure [type_string] (
  add_exception ident_not_found [] (
  add_exception ident_sys_error [type_string] (
  add_exception ident_end_of_file [] (
  add_exception ident_division_by_zero [] (
  (*e: [[Predef.build_initial_env()]] adding exceptions *)

  add_type ident_format decl_format (

  add_type ident_list decl_list (
  add_type ident_array decl_array (

  add_type ident_exn decl_exn (

  add_type ident_unit decl_unit (
  add_type ident_bool decl_bool (

  add_type ident_float decl_abstr (
  add_type ident_string decl_abstr (
  add_type ident_char decl_abstr (
  add_type ident_int decl_abstr (
  empty_env
  )))))))))))))))))))
(*e: function Predef.build_initial_env *)

(*s: constant Predef.builtin_values *)
let builtin_values =
  List.map (fun id -> Ident.make_global id; (Ident.name id, id))
      [ident_match_failure; ident_out_of_memory; ident_stack_overflow;
       ident_invalid_argument;
       ident_failure; ident_not_found; ident_sys_error; ident_end_of_file;
       ident_division_by_zero]
(*e: constant Predef.builtin_values *)
(*e: ./typing/predef.ml *)
