(*s: asmcomp/linearize.mli *)
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

(* $Id: linearize.mli,v 1.9 1997/03/07 15:32:26 xleroy Exp $ *)

(*s: type Linearize.label *)
(* Transformation of Mach code into a list of pseudo-instructions. *)

type label = int
(*e: type Linearize.label *)
(*s: signature Linearize.new_label *)
val new_label: unit -> label
(*e: signature Linearize.new_label *)

(*s: type Linearize.instruction *)
type instruction =
  { mutable desc: instruction_desc;
    next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    live: Reg.t Set.t }
(*e: type Linearize.instruction *)

(*s: type Linearize.instruction_desc *)
and instruction_desc =
    Lend
  | Lop of Mach.operation
  | Lreloadretaddr
  | Lreturn
  | Llabel of label
  | Lbranch of label
  | Lcondbranch of Mach.test * label
  | Lcondbranch3 of label option * label option * label option
  | Lswitch of label array
  | Lsetuptrap of label
  | Lpushtrap
  | Lpoptrap
  | Lraise
(*e: type Linearize.instruction_desc *)

(*s: signature Linearize.end_instr *)
val end_instr: instruction
(*e: signature Linearize.end_instr *)
(*s: signature Linearize.instr_cons *)
val instr_cons: 
  instruction_desc -> Reg.t array -> Reg.t array -> instruction -> instruction
(*e: signature Linearize.instr_cons *)

(*s: type Linearize.fundecl *)
type fundecl =
  { fun_name: string;
    fun_body: instruction;
    fun_fast: bool }
(*e: type Linearize.fundecl *)

(*s: signature Linearize.fundecl *)
val fundecl: Mach.fundecl -> fundecl
(*e: signature Linearize.fundecl *)

(*e: asmcomp/linearize.mli *)
