(*s: ./typing/env.mli *)
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

(* $Id: env.mli,v 1.17 1997/05/15 13:30:02 xleroy Exp $ *)

(* Environment handling *)

open Types

type t

(*s: signature Env.empty *)
val empty: t
(*e: signature Env.empty *)
(*s: signature Env.initial *)
val initial: t
(*e: signature Env.initial *)

(*s: signature Env.find_value *)
(* Lookup by paths *)

val find_value: Path.t -> t -> value_description
(*e: signature Env.find_value *)
(*s: signature Env.find_type *)
val find_type: Path.t -> t -> type_declaration
(*e: signature Env.find_type *)
(*s: signature Env.find_module *)
val find_module: Path.t -> t -> module_type
(*e: signature Env.find_module *)
(*s: signature Env.find_modtype *)
val find_modtype: Path.t -> t -> modtype_declaration
(*e: signature Env.find_modtype *)

(*s: signature Env.find_type_expansion *)
val find_type_expansion: Path.t -> t -> type_expr list * type_expr
(*e: signature Env.find_type_expansion *)
(*s: signature Env.find_modtype_expansion *)
val find_modtype_expansion: Path.t -> t -> Types.module_type
(*e: signature Env.find_modtype_expansion *)

(*s: signature Env.lookup_value *)
(* Lookup by long identifiers *)

val lookup_value: Longident.t -> t -> Path.t * value_description
(*e: signature Env.lookup_value *)
(*s: signature Env.lookup_constructor *)
val lookup_constructor: Longident.t -> t -> constructor_description
(*e: signature Env.lookup_constructor *)
(*s: signature Env.lookup_label *)
val lookup_label: Longident.t -> t -> label_description
(*e: signature Env.lookup_label *)
(*s: signature Env.lookup_type *)
val lookup_type: Longident.t -> t -> Path.t * type_declaration
(*e: signature Env.lookup_type *)
(*s: signature Env.lookup_module *)
val lookup_module: Longident.t -> t -> Path.t * module_type
(*e: signature Env.lookup_module *)
(*s: signature Env.lookup_modtype *)
val lookup_modtype: Longident.t -> t -> Path.t * modtype_declaration
(*e: signature Env.lookup_modtype *)

(*s: signature Env.add_value *)
(* Insertion by identifier *)

val add_value: Ident.t -> value_description -> t -> t
(*e: signature Env.add_value *)
(*s: signature Env.add_type *)
val add_type: Ident.t -> type_declaration -> t -> t
(*e: signature Env.add_type *)
(*s: signature Env.add_exception *)
val add_exception: Ident.t -> exception_declaration -> t -> t
(*e: signature Env.add_exception *)
(*s: signature Env.add_module *)
val add_module: Ident.t -> module_type -> t -> t
(*e: signature Env.add_module *)
(*s: signature Env.add_modtype *)
val add_modtype: Ident.t -> modtype_declaration -> t -> t
(*e: signature Env.add_modtype *)

(*s: signature Env.add_item *)
(* Insertion of all fields of a signature. *)

val add_item: signature_item -> t -> t
(*e: signature Env.add_item *)
(*s: signature Env.add_signature *)
val add_signature: signature -> t -> t
(*e: signature Env.add_signature *)

(*s: signature Env.open_signature *)
(* Insertion of all fields of a signature, relative to the given path.
   Used to implement open. *)

val open_signature: Path.t -> signature -> t -> t
(*e: signature Env.open_signature *)
(*s: signature Env.open_pers_signature *)
val open_pers_signature: string -> t -> t
(*e: signature Env.open_pers_signature *)

(*s: signature Env.enter_value *)
(* Insertion by name *)

val enter_value: string -> value_description -> t -> Ident.t * t
(*e: signature Env.enter_value *)
(*s: signature Env.enter_exception *)
val enter_exception: string -> exception_declaration -> t -> Ident.t * t
(*e: signature Env.enter_exception *)
(*s: signature Env.enter_module *)
val enter_module: string -> module_type -> t -> Ident.t * t
(*e: signature Env.enter_module *)
(*s: signature Env.enter_modtype *)
val enter_modtype: string -> modtype_declaration -> t -> Ident.t * t
(*e: signature Env.enter_modtype *)

(*s: signature Env.reset_cache *)
(* Reset the cache of in-core module interfaces.
   To be called in particular when load_path changes. *)

val reset_cache: unit -> unit
(*e: signature Env.reset_cache *)

(*s: signature Env.read_signature *)
(* Read, save a signature to/from a file *)

val read_signature: string -> string -> signature
        (* Arguments: module name, file name. Results: signature. *)
(*e: signature Env.read_signature *)
(*s: signature Env.save_signature *)
val save_signature: signature -> string -> string -> unit
        (* Arguments: signature, module name, file name. *)
(*e: signature Env.save_signature *)

(*s: signature Env.imported_units *)
(* Return the set of compilation units imported, with their CRC *)

val imported_units: unit -> (string * Digest.t) list
(*e: signature Env.imported_units *)

(*s: type Env.summary *)
(* Summaries -- compact representation of an environment, to be
   exported in debugging information. *)

type summary =
    Env_empty
  | Env_value of summary * Ident.t * value_description
  | Env_type of summary * Ident.t * type_declaration
  | Env_exception of summary * Ident.t * exception_declaration
  | Env_module of summary * Ident.t * module_type
  | Env_modtype of summary * Ident.t * modtype_declaration
  | Env_open of summary * Path.t
(*e: type Env.summary *)

(*s: signature Env.summary *)
val summary: t -> summary
(*e: signature Env.summary *)

(*s: type Env.error *)
(* Error report *)

type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string
  | Inconsistent_import of string * string * string
(*e: type Env.error *)

(*s: exception Env.Error *)
exception Error of error
(*e: exception Env.Error *)

(*s: signature Env.report_error *)
val report_error: error -> unit
(*e: signature Env.report_error *)

(*s: signature Env.check_modtype_inclusion *)
(* Forward declaration to break mutual recursion with includemod. *)

val check_modtype_inclusion: (t -> module_type -> module_type -> unit) ref
(*e: signature Env.check_modtype_inclusion *)
(*e: ./typing/env.mli *)
