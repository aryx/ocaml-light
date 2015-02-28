(*s: ./typing/predef.mli *)
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

open Types

(*s: signatures Predef.type_xxx *)
val type_int    : type_expr
val type_char   : type_expr
val type_string : type_expr
val type_float  : type_expr
val type_bool   : type_expr
val type_unit   : type_expr

val type_list  : type_expr -> type_expr
val type_array : type_expr -> type_expr
(*x: signatures Predef.type_xxx *)
val type_exn: type_expr
(*e: signatures Predef.type_xxx *)

(*s: signatures Predef.path_xxx *)
val path_int: Path.t
val path_char: Path.t
val path_string: Path.t
val path_float: Path.t
val path_bool: Path.t
val path_unit: Path.t

val path_list: Path.t
val path_array: Path.t
(*x: signatures Predef.path_xxx *)
val path_exn: Path.t
(*x: signatures Predef.path_xxx *)
val path_format: Path.t
(*e: signatures Predef.path_xxx *)

(*s: signature Predef.path_match_failure *)
val path_match_failure: Path.t
(*e: signature Predef.path_match_failure *)

(*s: signature Predef.build_initial_env *)
(* To build the initial environment. Since there is a nasty mutual
   recursion between predef and env, we break it by parameterizing
   over Env.t, Env.add_type and Env.add_exception. *)

val build_initial_env:
  (Ident.t -> type_declaration -> 'a -> 'a) ->      (* add_type *)
  (Ident.t -> exception_declaration -> 'a -> 'a) -> (* add_exception *)
  'a -> 'a
(*e: signature Predef.build_initial_env *)

(*s: signature Predef.builtin_values *)
(* To initialize linker tables *)

val builtin_values: (string * Ident.t) list
(*e: signature Predef.builtin_values *)
(*e: ./typing/predef.mli *)
