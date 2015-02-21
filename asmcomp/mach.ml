(*s: asmcomp/mach.ml *)
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

(* $Id: mach.ml,v 1.14 1997/03/04 10:19:49 xleroy Exp $ *)

(*s: type Mach.integer_comparison (asmcomp/mach.ml) *)
(* Representation of machine code by sequences of pseudoinstructions *)

type integer_comparison =
    Isigned of Cmm.comparison
  | Iunsigned of Cmm.comparison
(*e: type Mach.integer_comparison (asmcomp/mach.ml) *)

(*s: type Mach.integer_operation (asmcomp/mach.ml) *)
type integer_operation =
    Iadd | Isub | Imul | Idiv | Imod
  | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
  | Icomp of integer_comparison
  | Icheckbound
(*e: type Mach.integer_operation (asmcomp/mach.ml) *)

(*s: type Mach.test (asmcomp/mach.ml) *)
type test =
    Itruetest
  | Ifalsetest
  | Iinttest of integer_comparison
  | Iinttest_imm of integer_comparison * int
  | Ifloattest of Cmm.comparison * bool
  | Ioddtest
  | Ieventest
(*e: type Mach.test (asmcomp/mach.ml) *)

(*s: type Mach.operation (asmcomp/mach.ml) *)
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
(*e: type Mach.operation (asmcomp/mach.ml) *)

(*s: type Mach.instruction (asmcomp/mach.ml) *)
type instruction =
  { desc: instruction_desc;
    next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    mutable live: Reg.t Set.t }
(*e: type Mach.instruction (asmcomp/mach.ml) *)

(*s: type Mach.instruction_desc (asmcomp/mach.ml) *)
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
(*e: type Mach.instruction_desc (asmcomp/mach.ml) *)

(*s: type Mach.fundecl (asmcomp/mach.ml) *)
type fundecl =
  { fun_name: string;
    fun_args: Reg.t array;
    fun_body: instruction;
    fun_fast: bool }
(*e: type Mach.fundecl (asmcomp/mach.ml) *)

(*s: constant Mach.dummy_instr *)
let rec dummy_instr =
  { desc = Iend; 
    next = dummy_instr;
    arg = [||]; 
    res = [||];
    live = (*Reg.*)Set.empty }
(*e: constant Mach.dummy_instr *)

(*s: function Mach.end_instr *)
let end_instr () =
  { desc = Iend; 
    next = dummy_instr;
    arg = [||]; 
    res = [||];
    live = (*Reg.*)Set.empty }
(*e: function Mach.end_instr *)

(*s: function Mach.instr_cons *)
let instr_cons d a r n =
  { desc = d; next = n; arg = a; res = r; live = (*Reg.*)Set.empty }
(*e: function Mach.instr_cons *)

(*s: function Mach.instr_cons_live *)
let instr_cons_live d a r l n =
  { desc = d; next = n; arg = a; res = r; live = l }
(*e: function Mach.instr_cons_live *)

(*s: function Mach.instr_iter *)
let rec instr_iter f i =
  match i.desc with
    Iend -> ()
  | _ ->
      f i;
      match i.desc with
        Iend -> ()
      | Ireturn | Iop(Itailcall_ind) | Iop(Itailcall_imm _) -> ()
      | Iifthenelse(tst, ifso, ifnot) ->
          instr_iter f ifso; instr_iter f ifnot; instr_iter f i.next
      | Iswitch(index, cases) ->
          for i = 0 to Array.length cases - 1 do
            instr_iter f cases.(i)
          done;
          instr_iter f i.next
      | Iloop(body) ->
          instr_iter f body; instr_iter f i.next
      | Icatch(body, handler) ->
          instr_iter f body; instr_iter f handler; instr_iter f i.next
      | Iexit -> ()
      | Itrywith(body, handler) ->
          instr_iter f body; instr_iter f handler; instr_iter f i.next
      | Iraise -> ()
      | _ ->
          instr_iter f i.next      
(*e: function Mach.instr_iter *)

(*e: asmcomp/mach.ml *)
