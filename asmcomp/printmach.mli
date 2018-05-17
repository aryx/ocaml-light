(*s: asmcomp/printmach.mli *)
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

(*s: signature [[Printmach.reg]] *)
(* Pretty-printing of pseudo machine code *)

val reg: Reg.t -> unit
(*e: signature [[Printmach.reg]] *)
(*s: signature [[Printmach.regs]] *)
val regs: Reg.t array -> unit
(*e: signature [[Printmach.regs]] *)
(*s: signature [[Printmach.regset]] *)
val regset: Reg.t Set.t -> unit
(*e: signature [[Printmach.regset]] *)
(*s: signature [[Printmach.regsetaddr]] *)
val regsetaddr: Reg.t Set.t -> unit
(*e: signature [[Printmach.regsetaddr]] *)
(*s: signature [[Printmach.operation]] *)
val operation: Mach.operation -> Reg.t array -> Reg.t array -> unit
(*e: signature [[Printmach.operation]] *)
(*s: signature [[Printmach.test]] *)
val test: Mach.test -> Reg.t array -> unit
(*e: signature [[Printmach.test]] *)
(*s: signature [[Printmach.instr]] *)
val instr: Mach.instruction -> unit
(*e: signature [[Printmach.instr]] *)
(*s: signature [[Printmach.fundecl]] *)
val fundecl: Mach.fundecl -> unit
(*e: signature [[Printmach.fundecl]] *)
(*s: signature [[Printmach.phase]] *)
val phase: string -> Mach.fundecl -> unit
(*e: signature [[Printmach.phase]] *)
(*s: signature [[Printmach.interferences]] *)
val interferences: unit -> unit
(*e: signature [[Printmach.interferences]] *)
(*s: signature [[Printmach.preferences]] *)
val preferences: unit -> unit
(*e: signature [[Printmach.preferences]] *)

(*s: signature [[Printmach.print_live]] *)
val print_live: bool ref
(*e: signature [[Printmach.print_live]] *)
(*e: asmcomp/printmach.mli *)
