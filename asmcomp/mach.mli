(*s: asmcomp/mach.mli *)
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

(*s: type Mach.integer_comparison *)
(* Representation of machine code by sequences of pseudoinstructions *)

type integer_comparison =
    Isigned of Cmm.comparison
  | Iunsigned of Cmm.comparison
(*e: type Mach.integer_comparison *)

(*s: type Mach.integer_operation *)
type integer_operation =
    Iadd | Isub | Imul | Idiv | Imod
  | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
  | Icomp of integer_comparison
  | Icheckbound
(*e: type Mach.integer_operation *)

(*s: type Mach.test *)
type test =
    Itruetest
  | Ifalsetest
  | Iinttest of integer_comparison
  | Iinttest_imm of integer_comparison * int
  | Ifloattest of Cmm.comparison * bool
  | Ioddtest
  | Ieventest
(*e: type Mach.test *)

(*s: type Mach.operation *)
type operation =
    Imove
  | Ispill
  | Ireload
  | Iconst_int of Nativeint.t
  | Iconst_float of string
  | Iconst_symbol of string
  | Icall_ind
  | Icall_imm of string
  | Itailcall_ind
  | Itailcall_imm of string
  | Iextcall of string * bool
  | Istackoffset of int
  | Iload of Cmm.memory_chunk * Arch.addressing_mode
  | Istore of Cmm.memory_chunk * Arch.addressing_mode
  | Ialloc of int
  | Iintop of integer_operation
  | Iintop_imm of integer_operation * int
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Ifloatofint | Iintoffloat
  | Ispecific of Arch.specific_operation
(*e: type Mach.operation *)

(*s: type Mach.instruction *)
type instruction =
  { desc: instruction_desc;
    next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    mutable live: Reg.t Set.t }
(*e: type Mach.instruction *)

(*s: type Mach.instruction_desc *)
and instruction_desc =
    Iend
  | Iop of operation
  | Ireturn
  | Iifthenelse of test * instruction * instruction
  | Iswitch of int array * instruction array
  | Iloop of instruction
  | Icatch of instruction * instruction
  | Iexit
  | Itrywith of instruction * instruction
  | Iraise
(*e: type Mach.instruction_desc *)

(*s: type Mach.fundecl *)
type fundecl =
  { fun_name: string;
    fun_args: Reg.t array;
    fun_body: instruction;
    fun_fast: bool }
(*e: type Mach.fundecl *)

(*s: signature Mach.dummy_instr *)
val dummy_instr: instruction
(*e: signature Mach.dummy_instr *)
(*s: signature Mach.end_instr *)
val end_instr: unit -> instruction
(*e: signature Mach.end_instr *)
(*s: signature Mach.instr_cons *)
val instr_cons: 
      instruction_desc -> Reg.t array -> Reg.t array -> instruction ->
        instruction
(*e: signature Mach.instr_cons *)
(*s: signature Mach.instr_cons_live *)
val instr_cons_live: 
      instruction_desc -> Reg.t array -> Reg.t array -> Reg.t Set.t ->
        instruction -> instruction
(*e: signature Mach.instr_cons_live *)
(*s: signature Mach.instr_iter *)
val instr_iter: (instruction -> unit) -> instruction -> unit
(*e: signature Mach.instr_iter *)

(*e: asmcomp/mach.mli *)
