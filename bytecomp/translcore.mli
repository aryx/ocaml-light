(*s: ./bytecomp/translcore.mli *)
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

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Asttypes
open Types
open Typedtree
open Lambda

(*s: signature [[Translcore.name_pattern]] *)
val name_pattern: string -> (pattern * 'a) list -> Ident.t
(*e: signature [[Translcore.name_pattern]] *)
(*s: signature [[Translcore.maybe_pointer]] *)
val maybe_pointer: expression -> bool
(*e: signature [[Translcore.maybe_pointer]] *)

(*s: signature [[Translcore.transl_exp]] *)
val transl_exp: Typedtree.expression -> lambda
(*e: signature [[Translcore.transl_exp]] *)
(*s: signature [[Translcore.transl_let]] *)
val transl_let:
      rec_flag -> (Typedtree.pattern * Typedtree.expression) list -> 
        lambda -> lambda
(*e: signature [[Translcore.transl_let]] *)
(*s: signature [[Translcore.transl_primitive]] *)
val transl_primitive: Primitive.description -> lambda
(*e: signature [[Translcore.transl_primitive]] *)
(*s: signature [[Translcore.transl_exception]] *)
val transl_exception: Ident.t -> exception_declaration -> lambda
(*e: signature [[Translcore.transl_exception]] *)

(*s: type [[Translcore.error]] *)
type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr
(*e: type [[Translcore.error]] *)

(*s: exception [[Translcore.Error]] *)
exception Error of Location.t * error
(*e: exception [[Translcore.Error]] *)

(*s: signature [[Translcore.report_error]] *)
val report_error: error -> unit
(*e: signature [[Translcore.report_error]] *)
(*e: ./bytecomp/translcore.mli *)
