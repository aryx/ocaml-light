(*s: ./typing/env.mli *)
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

(* Environment handling *)

open Types

(*s: signature type Env.t *)
type t
(*e: signature type Env.t *)

(*s: signature Env.empty *)
val empty: t
(*e: signature Env.empty *)
(*s: signature Env.initial *)
val initial: t
(*e: signature Env.initial *)

(*s: signature Env.find_value *)
val find_value: Path.t -> t -> Types.value_description
(*e: signature Env.find_value *)
(*s: signature Env.find_type *)
val find_type: Path.t -> t -> Types.type_declaration
(*e: signature Env.find_type *)

(*s: signature Env.lookup_value *)
val lookup_value: Longident.t -> t -> Path.t * Types.value_description
(*e: signature Env.lookup_value *)
(*s: signature Env.lookup_constructor *)
val lookup_constructor: Longident.t -> t -> Types.constructor_description
(*e: signature Env.lookup_constructor *)
(*s: signature Env.lookup_label *)
val lookup_label: Longident.t -> t -> Types.label_description
(*e: signature Env.lookup_label *)
(*s: signature Env.lookup_type *)
val lookup_type: Longident.t -> t -> Path.t * Types.type_declaration
(*e: signature Env.lookup_type *)
(*s: signature Env.lookup_module *)
val lookup_module: Longident.t -> t -> Path.t * Types.module_type
(*e: signature Env.lookup_module *)

(* Insertion by identifier *)

(*s: signature Env.add_value *)
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

(*s: signature Env.add_signature *)
(* Insertion of all fields of a signature. *)

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

(* Insertion by name *)

(*s: signature Env.enter_value *)
val enter_value: string -> value_description -> t -> Ident.t * t
(*e: signature Env.enter_value *)
(*s: signature Env.enter_type *)
val enter_type: string -> type_declaration -> t -> Ident.t * t
(*e: signature Env.enter_type *)
(*s: signature Env.enter_exception *)
val enter_exception: string -> exception_declaration -> t -> Ident.t * t
(*e: signature Env.enter_exception *)
(*s: signature Env.enter_module *)
val enter_module: string -> module_type -> t -> Ident.t * t
(*e: signature Env.enter_module *)

(*s: signature Env.reset_cache *)
(* Reset the cache of in-core module interfaces.
   To be called in particular when load_path changes. *)
val reset_cache: unit -> unit
(*e: signature Env.reset_cache *)

(*s: signature Env.read_signature *)
(* Read, save a signature to/from a file *)

val read_signature: string -> string -> Types.signature * Digest.t
        (* Arguments: module name, file name.
           Results: signature, CRC. *)
(*e: signature Env.read_signature *)
(*s: signature Env.save_signature *)
val save_signature: signature -> string -> string -> Digest.t
        (* Arguments: signature, module name, file name.
           Result: CRC. *)
(*e: signature Env.save_signature *)

(*s: signature Env.imported_units *)
(* Return the set of compilation units imported, with their CRC *)

val imported_units: unit -> (string * Digest.t) list
(*e: signature Env.imported_units *)

(*s: type Env.error *)
(* Error report *)

type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string
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
