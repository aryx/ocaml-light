(*s: ./typing/includemod.mli *)
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

(* $Id$ *)

(* Inclusion checks for the module language *)

open Types
open Typedtree

(*s: signature Includemod.modtypes *)
val modtypes: Env.t -> module_type -> module_type -> module_coercion
(*e: signature Includemod.modtypes *)
(*s: signature Includemod.signatures *)
val signatures: Env.t -> signature -> signature -> module_coercion
(*e: signature Includemod.signatures *)
(*s: signature Includemod.compunit *)
val compunit: string -> signature -> string -> signature -> module_coercion
(*e: signature Includemod.compunit *)

(*s: type Includemod.error *)
type error =
    Missing_field of Ident.t
  | Value_descriptions of Ident.t * value_description * value_description
  | Type_declarations of Ident.t * type_declaration * type_declaration
  | Exception_declarations of
      Ident.t * exception_declaration * exception_declaration
  | Module_types of module_type * module_type
  | Interface_mismatch of string * string
(*e: type Includemod.error *)

(*s: exception Includemod.Error *)
exception Error of error list
(*e: exception Includemod.Error *)

(*s: signature Includemod.report_error *)
val report_error: error list -> unit
(*e: signature Includemod.report_error *)
(*e: ./typing/includemod.mli *)
