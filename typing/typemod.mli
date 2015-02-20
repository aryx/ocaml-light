(*s: ./typing/typemod.mli *)
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

(* Type-checking of the module language *)

open Types

(*s: signature Typemod.type_structure *)
val type_structure:
        Env.t -> Parsetree.structure -> Typedtree.structure * signature * Env.t
(*e: signature Typemod.type_structure *)
(*s: signature Typemod.transl_signature *)
val transl_signature:
        Env.t -> Parsetree.signature -> signature
(*e: signature Typemod.transl_signature *)
(*s: signature Typemod.check_nongen_schemes *)
val check_nongen_schemes:
        Env.t -> Typedtree.structure -> unit
(*e: signature Typemod.check_nongen_schemes *)

(*s: type Typemod.error *)
type error =
    Unbound_module of Longident.t
  | Unbound_modtype of Longident.t
  | Not_included of Includemod.error list
  | Signature_expected
  | Structure_expected of module_type
  | With_no_component of Longident.t
  | With_mismatch of Longident.t * Includemod.error list
  | Repeated_name of string * string
  | Non_generalizable of type_expr
  | Non_generalizable_module of module_type
(*e: type Typemod.error *)

(*s: exception Typemod.Error *)
exception Error of Location.t * error
(*e: exception Typemod.Error *)

(*s: signature Typemod.report_error *)
val report_error: error -> unit
(*e: signature Typemod.report_error *)
(*e: ./typing/typemod.mli *)
