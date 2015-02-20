(*s: ./typing/predef.mli *)
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

(* $Id: predef.mli,v 1.6 1996/09/23 11:33:10 xleroy Exp $ *)

(* Predefined type constructors (with special typing rules in typecore) *)

open Types

(*s: signature Predef.type_int *)
val type_int: type_expr
(*e: signature Predef.type_int *)
(*s: signature Predef.type_char *)
val type_char: type_expr
(*e: signature Predef.type_char *)
(*s: signature Predef.type_string *)
val type_string: type_expr
(*e: signature Predef.type_string *)
(*s: signature Predef.type_float *)
val type_float: type_expr
(*e: signature Predef.type_float *)
(*s: signature Predef.type_bool *)
val type_bool: type_expr
(*e: signature Predef.type_bool *)
(*s: signature Predef.type_unit *)
val type_unit: type_expr
(*e: signature Predef.type_unit *)
(*s: signature Predef.type_exn *)
val type_exn: type_expr
(*e: signature Predef.type_exn *)
(*s: signature Predef.type_array *)
val type_array: type_expr -> type_expr
(*e: signature Predef.type_array *)
(*s: signature Predef.type_list *)
val type_list: type_expr -> type_expr
(*e: signature Predef.type_list *)

(*s: signature Predef.path_int *)
val path_int: Path.t
(*e: signature Predef.path_int *)
(*s: signature Predef.path_char *)
val path_char: Path.t
(*e: signature Predef.path_char *)
(*s: signature Predef.path_string *)
val path_string: Path.t
(*e: signature Predef.path_string *)
(*s: signature Predef.path_float *)
val path_float: Path.t
(*e: signature Predef.path_float *)
(*s: signature Predef.path_bool *)
val path_bool: Path.t
(*e: signature Predef.path_bool *)
(*s: signature Predef.path_unit *)
val path_unit: Path.t
(*e: signature Predef.path_unit *)
(*s: signature Predef.path_exn *)
val path_exn: Path.t
(*e: signature Predef.path_exn *)
(*s: signature Predef.path_array *)
val path_array: Path.t
(*e: signature Predef.path_array *)
(*s: signature Predef.path_list *)
val path_list: Path.t
(*e: signature Predef.path_list *)
(*s: signature Predef.path_format *)
val path_format: Path.t
(*e: signature Predef.path_format *)

(*s: signature Predef.path_match_failure *)
val path_match_failure: Path.t
(*e: signature Predef.path_match_failure *)

(*s: signature Predef.build_initial_env *)
(* To build the initial environment. Since there is a nasty mutual
   recursion between predef and env, we break it by parameterizing
   over Env.t, Env.add_type and Env.add_exception. *)

val build_initial_env:
  (Ident.t -> type_declaration -> 'a -> 'a) ->
  (Ident.t -> exception_declaration -> 'a -> 'a) ->
  'a -> 'a
(*e: signature Predef.build_initial_env *)

(*s: signature Predef.builtin_values *)
(* To initialize linker tables *)

val builtin_values: (string * Ident.t) list
(*e: signature Predef.builtin_values *)
(*e: ./typing/predef.mli *)
