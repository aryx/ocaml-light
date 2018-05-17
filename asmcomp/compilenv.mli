(*s: asmcomp/compilenv.mli *)
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

(* Compilation environments for compilation units *)

open Clambda

(*s: type [[Compilenv.unit_infos]] *)
type unit_infos =
  { mutable ui_name: string;                    (* Name of unit implemented *)
    mutable ui_imports_cmi: (string * Digest.t) list; (* Interfaces imported *)
    mutable ui_imports_cmx: (string * Digest.t) list; (* Infos imported *)
    mutable ui_approx: value_approximation;     (* Approx of the structure *)
    mutable ui_curry_fun: int list;             (* Currying functions needed *)
    mutable ui_apply_fun: int list;             (* Apply functions needed *)
    mutable ui_force_link: bool }               (* Always linked *)
(*e: type [[Compilenv.unit_infos]] *)

(*s: signature [[Compilenv.reset]] *)
val reset: string -> unit
        (* Reset the environment and record the name of the unit being
           compiled (arg). *)
(*e: signature [[Compilenv.reset]] *)

(*s: signature [[Compilenv.current_unit_name]] *)
val current_unit_name: unit -> string
        (* Return the name of the unit being compiled *)
(*e: signature [[Compilenv.current_unit_name]] *)

(*s: signature [[Compilenv.global_approx]] *)
val global_approx: Ident.t -> Clambda.value_approximation
        (* Return the approximation for the given global identifier *)
(*e: signature [[Compilenv.global_approx]] *)
(*s: signature [[Compilenv.set_global_approx]] *)
val set_global_approx: Clambda.value_approximation -> unit
        (* Record the approximation of the unit being compiled *)
(*e: signature [[Compilenv.set_global_approx]] *)

(*s: signature [[Compilenv.need_curry_fun]] *)
val need_curry_fun: int -> unit
(*e: signature [[Compilenv.need_curry_fun]] *)
(*s: signature [[Compilenv.need_apply_fun]] *)
val need_apply_fun: int -> unit
        (* Record the need of a currying (resp. application) function
           with the given arity *)
(*e: signature [[Compilenv.need_apply_fun]] *)

(*s: signature [[Compilenv.read_unit_info]] *)
val read_unit_info: string -> unit_infos * Digest.t
        (* Read infos and CRC from a [.cmx] file. *)
(*e: signature [[Compilenv.read_unit_info]] *)
(*s: signature [[Compilenv.save_unit_info]] *)
val save_unit_info: string -> unit
        (* Save the infos for the current unit in the given file *)
(*e: signature [[Compilenv.save_unit_info]] *)

(*s: signature [[Compilenv.cmx_not_found_crc]] *)
val cmx_not_found_crc: Digest.t
        (* Special digest used in the [ui_imports_cmx] list to signal
           that no [.cmx] file was found and used for the imported unit *)
(*e: signature [[Compilenv.cmx_not_found_crc]] *)

(*s: type [[Compilenv.error]] *)
type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of string * string
(*e: type [[Compilenv.error]] *)

(*s: exception [[Compilenv.Error]] *)
exception Error of error
(*e: exception [[Compilenv.Error]] *)

(*s: signature [[Compilenv.report_error]] *)
val report_error: error -> unit
(*e: signature [[Compilenv.report_error]] *)
(*e: asmcomp/compilenv.mli *)
