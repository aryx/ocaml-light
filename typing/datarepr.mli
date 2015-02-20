(*s: ./typing/datarepr.mli *)
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

(* $Id: datarepr.mli,v 1.4 1996/09/23 11:33:01 xleroy Exp $ *)

(* Compute constructor and label descriptions from type declarations,
   determining their representation. *)

open Asttypes
open Types

(*s: signature Datarepr.constructor_descrs *)
val constructor_descrs:
  type_expr -> (string * type_expr list) list ->
    (string * constructor_description) list
(*e: signature Datarepr.constructor_descrs *)
(*s: signature Datarepr.exception_descr *)
val exception_descr:
  Path.t -> type_expr list -> constructor_description
(*e: signature Datarepr.exception_descr *)
(*s: signature Datarepr.label_descrs *)
val label_descrs:
  type_expr -> (string * mutable_flag * type_expr) list ->
    (string * label_description) list
(*e: signature Datarepr.label_descrs *)
(*e: ./typing/datarepr.mli *)
