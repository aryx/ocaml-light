(*s: yacc2/check.ml *)
(* Yoann Padioleau
 *
 * Copyright (C) 2015 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* TODO:
 * - exist 'start', and 'type' directives
 * - classic use/def: remember set of terms and non terms and look
 *   for use of undefined symbols, or unused symbols.
 * - wrong $ number, too big
 * - typechecking (but this is done for free by ocaml in the generated code)
 *)

(*****************************************************************************)
(* Types *)
(*s: enum Check.error (yacc2/check.ml) (yacc) *)
(*****************************************************************************)

type error = unit
(*e: enum Check.error (yacc2/check.ml) (yacc) *)

(*s: exception Check.Error (yacc2/check.ml) (yacc) *)
exception Error of error
(*e: exception Check.Error (yacc2/check.ml) (yacc) *)

(*****************************************************************************)
(* Helpers *)
(*s: function Check.report_error (yacc) *)
(*****************************************************************************)

let report_error err =
  failwith "TODO"
(*e: function Check.report_error (yacc) *)

(*****************************************************************************)
(* Main entry point *)
(*s: function Check.check (yacc) *)
(*****************************************************************************)

let check def =
  failwith "TODO"
(*e: function Check.check (yacc) *)

(*e: yacc2/check.ml *)
