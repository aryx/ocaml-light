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

(* $Id: schedgen.mli,v 1.2 1997/11/13 10:57:09 xleroy Exp $ *)

(* Instruction scheduling *)

type code_dag_node =
  { instr: Linearize.instruction;
    delay: int;
    mutable sons: (code_dag_node * int) list;
    mutable date: int;
    mutable length: int;
    mutable ancestors: int;
    mutable emitted_ancestors: int }

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
