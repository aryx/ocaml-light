(*s: asmcomp/schedgen.mli *)
(*s: copyright header 1997 *)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header 1997 *)

(*s: type Schedgen.code_dag_node *)
(* Instruction scheduling *)

type code_dag_node =
  { instr: Linearize.instruction;
    delay: int;
    mutable sons: (code_dag_node * int) list;
    mutable date: int;
    mutable length: int;
    mutable ancestors: int;
    mutable emitted_ancestors: int }
(*e: type Schedgen.code_dag_node *)

(*s: type Schedgen.scheduler *)
type scheduler = {
  (* old: virtual *)
  (* Can be overriden by processor description *)
  oper_issue_cycles : Mach.operation -> int;
      (* Number of cycles needed to issue the given operation *)
  oper_latency : Mach.operation -> int;
      (* Number of cycles needed to complete the given operation *)
  oper_in_basic_block : Mach.operation -> bool;
      (* Says whether the given operation terminates a basic block *)

  (* Entry point *)
  schedule_fundecl : 
    scheduler ->
    Linearize.fundecl -> Linearize.fundecl;

  (* old: protected *)

  instr_in_basic_block: 
   scheduler -> Linearize.instruction -> bool;
  instr_latency:
   scheduler -> Linearize.instruction -> int;
  instr_issue_cycles:
   scheduler -> Linearize.instruction -> int;
  add_instruction:
   scheduler -> 
   code_dag_node list -> Linearize.instruction -> code_dag_node list;
  ready_instruction:
   int -> code_dag_node list -> code_dag_node option;
  reschedule:  
   scheduler -> 
   code_dag_node list -> int -> Linearize.instruction -> Linearize.instruction;
 
}
(*e: type Schedgen.scheduler *)

(*s: signature Schedgen.scheduler_generic *)
val scheduler_generic: unit -> scheduler
(*e: signature Schedgen.scheduler_generic *)

(*
  oper_issue_cycles = super.oper_issue_cycles;
  oper_latency = super.oper_latency;
  oper_in_basic_block = super.oper_in_basic_block;
  schedule_fundecl = super.schedule_fundecl;
  instr_in_basic_block = super.instr_in_basic_block;
  instr_latency = super.instr_latency;
  instr_issue_cycles = super.instr_issue_cycles;
  add_instruction = super.add_instruction;
  ready_instruction = super.ready_instruction;
  reschedule = super.reschedule;
*)
(*e: asmcomp/schedgen.mli *)
