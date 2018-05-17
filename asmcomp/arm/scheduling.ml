(*s: asmcomp/arm/scheduling.ml *)
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
open Mach

open Schedgen

(*s: function [[Scheduling.scheduler]] *)
(* Instruction scheduling for the Sparc *)

let scheduler () = 

  let super = Schedgen.scheduler_generic () in
  {

  oper_in_basic_block = super.oper_in_basic_block;
  schedule_fundecl = super.schedule_fundecl;
  instr_in_basic_block = super.instr_in_basic_block;
  instr_latency = super.instr_latency;
  instr_issue_cycles = super.instr_issue_cycles;
  add_instruction = super.add_instruction;
  ready_instruction = super.ready_instruction;
  reschedule = super.reschedule;

(* Scheduling -- based roughly on the Strong ARM *)

 oper_latency = (function
    Ireload -> 2
  | Iload(_, _) -> 2
  | Iconst_symbol _ -> 2                (* turned into a load *)
  | Iconst_float _ -> 2                 (* turned into a load *)
  | Iintop(Imul) -> 3
  | Iintop_imm(Imul, _) -> 3
  (* No data available for floatops, let's make educated guesses *)
  | Iaddf -> 3
  | Isubf -> 3
  | Imulf -> 5
  | Idivf -> 15
  | _ -> 1
 );

(* Issue cycles.  Rough approximations *)

 oper_issue_cycles = (function
    Ialloc _ -> 4
  | Iintop(Icomp _) -> 3
  | Iintop(Icheckbound) -> 2
  | Iintop_imm(Idiv, _) -> 4
  | Iintop_imm(Imod, _) -> 6
  | Iintop_imm(Icomp _, _) -> 3
  | Iintop_imm(Icheckbound, _) -> 2
  | _ -> 1
 );
  }
(*e: function [[Scheduling.scheduler]] *)

(*s: function [[Scheduling.fundecl]] *)
let fundecl f = 
  let s = scheduler () in
  s.schedule_fundecl s f
(*e: function [[Scheduling.fundecl]] *)
(*e: asmcomp/arm/scheduling.ml *)
