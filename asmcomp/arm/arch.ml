(*s: asmcomp/arm/arch.ml *)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Specific operations for the ARM processor *)

open Misc
open Format

(*s: type Arch.addressing_mode *)
(* Addressing modes *)

type addressing_mode =
    Iindexed of int                     (* reg + displ *)
(*e: type Arch.addressing_mode *)

(* We do not support the reg + shifted reg addressing mode, because
   what we really need is reg + shifted reg + displ,
   and this is decomposed in two instructions (reg + shifted reg -> tmp,
   then addressing tmp + displ). *)

(* Specific operations *)

(*s: type Arch.specific_operation *)
type specific_operation =
    Ishiftarith of arith_operation * int
  | Ishiftcheckbound of int
  | Irevsubimm of int
(*e: type Arch.specific_operation *)

(*s: type Arch.arith_operation *)
and arith_operation =
    Ishiftadd
  | Ishiftsub
  | Ishiftsubrev
(*e: type Arch.arith_operation *)

(*s: type Arch.float_operation *)
type float_operation = unit
(*e: type Arch.float_operation *)

(*s: constant Arch.big_endian *)
(* Sizes, endianness *)

let big_endian = false
(*e: constant Arch.big_endian *)

(*s: constant Arch.size_addr *)
let size_addr = 4
(*e: constant Arch.size_addr *)
(*s: constant Arch.size_int *)
let size_int = 4
(*e: constant Arch.size_int *)
(*s: constant Arch.size_float *)
let size_float = 8
(*e: constant Arch.size_float *)

(*s: constant Arch.identity_addressing *)
(* Operations on addressing modes *)

let identity_addressing = Iindexed 0
(*e: constant Arch.identity_addressing *)

(*s: function Arch.offset_addressing *)
let offset_addressing (Iindexed n) delta = Iindexed(n + delta)
(*e: function Arch.offset_addressing *)

(*s: function Arch.num_args_addressing *)
let num_args_addressing (Iindexed n) = 1
(*e: function Arch.num_args_addressing *)

(*s: function Arch.print_addressing *)
(* Printing operations and addressing modes *)

let print_addressing printreg addr arg =
  match addr with
    Iindexed n ->
      printreg arg.(0);
      if n <> 0 then begin print_string " + "; print_int n end
(*e: function Arch.print_addressing *)

(*s: function Arch.print_specific_operation *)
let print_specific_operation printreg op arg =
  match op with
    Ishiftarith(op, shift) ->
      printreg arg.(0);
      begin match op with
        Ishiftadd -> print_string " + "
      | Ishiftsub -> print_string " - "
      | Ishiftsubrev -> print_string " -rev "
      end;
      printreg arg.(1);
      if shift >= 0
      then begin print_string " << "; print_int shift end
      else begin print_string " >> "; print_int (-shift) end
  | Ishiftcheckbound n ->
      print_string "check ";
      printreg arg.(0);
      print_string " >> "; print_int n; print_string " > ";
      printreg arg.(1)
  | Irevsubimm n ->
      print_int n; print_string " - "; printreg arg.(0)
(*e: function Arch.print_specific_operation *)
(*e: asmcomp/arm/arch.ml *)
