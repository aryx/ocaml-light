(*s: ./bytecomp/translmod.mli *)
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
   for the module language *)

open Typedtree
open Lambda

(*s: signature Translmod.transl_implementation *)
val transl_implementation: string -> structure -> module_coercion -> lambda
(*e: signature Translmod.transl_implementation *)
(*s: signature Translmod.transl_store_implementation *)
val transl_store_implementation:
      string -> structure -> module_coercion -> int * lambda
(*e: signature Translmod.transl_store_implementation *)
(*s: signature Translmod.transl_toplevel_definition *)
val transl_toplevel_definition: structure -> lambda
(*e: signature Translmod.transl_toplevel_definition *)

(*s: signature Translmod.primitive_declarations *)
val primitive_declarations: string list ref
(*e: signature Translmod.primitive_declarations *)
(*e: ./bytecomp/translmod.mli *)
