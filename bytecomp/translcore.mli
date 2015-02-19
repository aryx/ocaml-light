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

(* $Id: translcore.mli,v 1.9 1996/09/23 11:30:26 xleroy Exp $ *)

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Asttypes
open Types
open Typedtree
open Lambda

val name_pattern: string -> (pattern * 'a) list -> Ident.t
val maybe_pointer: expression -> bool

val transl_exp: expression -> lambda
val transl_let:
      rec_flag -> (pattern * expression) list -> lambda -> lambda
val transl_primitive: Primitive.description -> lambda
val transl_exception: Ident.t -> exception_declaration -> lambda

type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr

exception Error of Location.t * error

val report_error: error -> unit
