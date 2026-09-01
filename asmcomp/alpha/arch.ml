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

(* Specific operations for the Alpha processor *)

open Format

(* Addressing modes *)

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)

(* Specific operations *)

type specific_operation =
    Iadd4 | Iadd8 | Isub4 | Isub8       (* Scaled adds and subs *)
  | Ireloadgp of bool                   (* The ldgp instruction *)

(* claude: required by Selectgen.selector's generic select_floatarith,
   used by architectures (e.g. i386) whose float unit has commutative-op
   swapping quirks. Alpha has no such quirk, so this is unused. *)
type float_operation = unit

(* Sizes, endianness *)

let big_endian = false

let size_addr = 8
let size_int = 8
let size_float = 8

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Ibased(s, n) -> Ibased(s, n + delta)
  | Iindexed n -> Iindexed(n + delta)

let num_args_addressing = function
    Ibased(s, n) -> 0
  | Iindexed n -> 1

(* Printing operations and addressing modes *)

let print_addressing printreg addr arg =
  match addr with
    Ibased(s, n) ->
      print_string "\""; print_string s; print_string "\"";
      if n <> 0 then begin print_string " + "; print_int n end
  | Iindexed n ->
      printreg arg.(0);
      if n <> 0 then begin print_string " + "; print_int n end

let print_specific_operation printreg op arg =
  match op with
    Iadd4 -> printreg arg.(0); print_string " * 4 + "; printreg arg.(1)
  | Iadd8 -> printreg arg.(0); print_string " * 8 + "; printreg arg.(1)
  | Isub4 -> printreg arg.(0); print_string " * 4 - "; printreg arg.(1)
  | Isub8 -> printreg arg.(0); print_string " * 8 - "; printreg arg.(1)
  | Ireloadgp _ -> print_string "ldgp"

(* claude: ocaml-light only wires up the alpha-linux-gnu- cross toolchain
   (see configure's -target-arch alpha), i.e. gas, never the Digital/OSF1
   assembler -- unlike the original code (which detected this from
   Config.system), this is hardcoded false. Kept as a named constant
   (rather than deleting the digital_asm-guarded branches below and in
   proc.ml/scheduling.ml/selection.ml) since those branches document real
   differences between the two assemblers. *)
let digital_asm = false
