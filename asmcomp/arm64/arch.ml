(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                  Benedikt Meurer, University of Siegen              *)
(*                                                                     *)
(*    Copyright 2013 Institut National de Recherche en Informatique    *)
(*    et en Automatique. Copyright 2012 Benedikt Meurer. All rights    *)
(*    reserved.  This file is distributed  under the terms of the Q    *)
(*    Public License version 1.0.                                      *)
(*                                                                     *)
(***********************************************************************)

let command_line_options = []

(* Specific operations for the ARM processor, 64-bit mode *)

(* Addressing modes *)

type addressing_mode =
  | Iindexed of int                     (* reg + displ *)
  | Ibased of string * int              (* global var + displ *)

(* We do not support the reg + shifted reg addressing mode, because
   what we really need is reg + shifted reg + displ,
   and this is decomposed in two instructions (reg + shifted reg -> tmp,
   then addressing tmp + displ). *)

(* Specific operations *)

type specific_operation =
  | Ishiftarith of arith_operation * int
  | Ishiftcheckbound of int
  | Imuladd       (* multiply and add *)
  | Imulsub       (* multiply and subtract *)
  | Inegmulf      (* floating-point negate and multiply *)
  | Imuladdf      (* floating-point multiply and add *)
  | Inegmuladdf   (* floating-point negate, multiply and add *)
  | Imulsubf      (* floating-point multiply and subtract *)
  | Inegmulsubf   (* floating-point negate, multiply and subtract *)
  | Isqrtf        (* floating-point square root *)
  | Ibswap of int (* endianess conversion *)

and arith_operation =
    Ishiftadd
  | Ishiftsub

(* claude: required by Selectgen.selector's generic select_floatarith
   field (see the same addition for every other arch here) -- arm64's
   own selection.ml never uses it, this fork has no "float arith with a
   memory operand" concept for a load/store architecture like ARM64. *)
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
  | Iindexed n -> Iindexed(n + delta)
  | Ibased(s, n) -> Ibased(s, n + delta)

let num_args_addressing = function
  | Iindexed n -> 1
  | Ibased(s, n) -> 0

(* Printing operations and addressing modes.
   claude: this fork's Printmach.ml calls Arch.print_addressing/
   print_specific_operation with a plain "Reg.t -> unit" printer and no
   Format.formatter (unlike upstream 4.02, which threads a ppf through --
   see the same adaptation for amd64/i386's arch.ml). *)

let print_addressing printreg addr arg =
  match addr with
  | Iindexed n ->
      printreg arg.(0);
      if n <> 0 then begin print_string " + "; print_int n end
  | Ibased(s, 0) ->
      print_string "\""; print_string s; print_string "\""
  | Ibased(s, n) ->
      print_string "\""; print_string s; print_string "\" + "; print_int n

let print_specific_operation printreg op arg =
  match op with
  | Ishiftarith(op, shift) ->
      printreg arg.(0);
      print_string (match op with Ishiftadd -> " + " | Ishiftsub -> " - ");
      printreg arg.(1);
      if shift >= 0
      then begin print_string " << "; print_int shift end
      else begin print_string " >> "; print_int (-shift) end
  | Ishiftcheckbound n ->
      print_string "check "; printreg arg.(0);
      print_string " >> "; print_int n; print_string " > "; printreg arg.(1)
  | Imuladd ->
      print_string "("; printreg arg.(0); print_string " * "; printreg arg.(1);
      print_string ") + "; printreg arg.(2)
  | Imulsub ->
      print_string "-("; printreg arg.(0); print_string " * "; printreg arg.(1);
      print_string ") + "; printreg arg.(2)
  | Inegmulf ->
      print_string "-f ("; printreg arg.(0); print_string " *f ";
      printreg arg.(1); print_string ")"
  | Imuladdf ->
      printreg arg.(0); print_string " +f ("; printreg arg.(1);
      print_string " *f "; printreg arg.(2); print_string ")"
  | Inegmuladdf ->
      print_string "(-f "; printreg arg.(0); print_string ") -f (";
      printreg arg.(1); print_string " *f "; printreg arg.(2); print_string ")"
  | Imulsubf ->
      printreg arg.(0); print_string " -f ("; printreg arg.(1);
      print_string " *f "; printreg arg.(2); print_string ")"
  | Inegmulsubf ->
      print_string "(-f "; printreg arg.(0); print_string ") +f (";
      printreg arg.(1); print_string " *f "; printreg arg.(2); print_string ")"
  | Isqrtf ->
      print_string "sqrtf "; printreg arg.(0)
  | Ibswap n ->
      print_string "bswap"; print_int n; print_string " "; printreg arg.(0)
