(*s: asmcomp/arm/selection.ml *)
(*s: copyright header 1998 *)
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
(*e: copyright header 1998 *)

(* Instruction selection for the ARM processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

open Selectgen

(*s: function Selection.is_immed *)
(* Immediate operands are 8-bit immediate values, zero-extended, and rotated
   right by 0, 2, 4, ... 30 bits.
   To avoid problems with Caml's 31-bit arithmetic,
   we check only with 8-bit values shifted left 0 to 22 bits. *)

let rec is_immed n shift =
  if shift > 22 then false
  else if n land (0xFF lsl shift) = n then true
  else is_immed n (shift + 2)
(*e: function Selection.is_immed *)

(*s: function Selection.is_offset *)
(* We have 12-bit signed offsets for word accesses,
   8-bit signed word offsets for float accesses,
   and 8-bit byte offsets for bytes and shorts.
   Use lowest common denominator. *)

let is_offset n = n < 128 && n > -128
(*e: function Selection.is_offset *)

(*s: function Selection.is_intconst *)
let is_intconst = function Cconst_int n -> true | _ -> false
(*e: function Selection.is_intconst *)




(*s: function Selection.select_shift_arith *)
let select_shift_arith super self = (fun op shiftop shiftrevop args ->
  match args with
    [arg1; Cop(Clsl, [arg2; Cconst_int n])]
    when n > 0 && n < 32 && not(is_intconst arg2) ->
      (Ispecific(Ishiftarith(shiftop, n)), [arg1; arg2])
  | [arg1; Cop(Casr, [arg2; Cconst_int n])]
    when n > 0 && n < 32 && not(is_intconst arg2) ->
      (Ispecific(Ishiftarith(shiftop, -n)), [arg1; arg2])
  | [Cop(Clsl, [arg1; Cconst_int n]); arg2]
    when n > 0 && n < 32 && not(is_intconst arg1) ->
      (Ispecific(Ishiftarith(shiftrevop, n)), [arg2; arg1])
  | [Cop(Casr, [arg1; Cconst_int n]); arg2]
    when n > 0 && n < 32 && not(is_intconst arg1) ->
      (Ispecific(Ishiftarith(shiftrevop, -n)), [arg2; arg1])
  | _ ->
      super.select_operation self op args
 )
(*e: function Selection.select_shift_arith *)


(*s: function Selection.selector *)
(* Instruction selection *)

let selector () = 

 let super = Selectgen.selector_generic () in
 {

  (* todo: super with feature needed ... *)
  select_condition = super.select_condition;
  select_store = super.select_store;
  emit_extcall_args = super.emit_extcall_args;
  emit_fundecl = super.emit_fundecl;
  extract = super.extract;
  insert = super.insert;
  insert_move = super.insert_move;
  insert_move_args = super.insert_move_args;
  insert_move_results = super.insert_move_results;
  insert_moves = super.insert_moves;
  emit_expr = super.emit_expr;
  emit_tail = super.emit_tail;
  select_arith_comm = super.select_arith_comm;
  select_arith = super.select_arith;
  select_shift = super.select_shift;
  select_arith_comp = super.select_arith_comp;
  emit_let = super.emit_let;
  emit_parts_list = super.emit_parts_list;
  emit_parts = super.emit_parts;
  emit_tuple = super.emit_tuple;
  emit_stores = super.emit_stores;
  emit_sequence = super.emit_sequence;
  emit_return = super.emit_return;
  emit_tail_sequence = super.emit_tail_sequence;
  select_floatarith = super.select_floatarith;
  select_push = super.select_push;

 is_immediate = (fun n ->
  n land 0xFF = n || is_immed n 2
 );

 select_addressing = (function
    Cop(Cadda, [arg; Cconst_int n]) when is_offset n ->
      (Iindexed n, arg)
  | Cop(Cadda, [arg1; Cop(Caddi, [arg2; Cconst_int n])]) when is_offset n ->
      (Iindexed n, Cop(Cadda, [arg1; arg2]))
  | arg ->
      (Iindexed 0, arg)
 );
 

 select_operation = (fun self op args ->
  match op with
    Cadda | Caddi ->
      begin match args with
        [arg1; Cconst_int n] when n < 0 && self.is_immediate (-n) ->
          (Iintop_imm(Isub, -n), [arg1])
      | _ ->
          select_shift_arith super self op Ishiftadd Ishiftadd args
      end
  | Csuba | Csubi ->
      begin match args with
        [arg1; Cconst_int n] when n < 0 && self.is_immediate (-n) ->
          (Iintop_imm(Iadd, -n), [arg1])
      | [Cconst_int n; arg2] when self.is_immediate n ->
          (Ispecific(Irevsubimm n), [arg2])
      | _ ->
          select_shift_arith super self op Ishiftsub Ishiftsubrev args
      end
  | Cmuli ->			(* no multiply immediate *)
      (Iintop Imul, args)
  | Cdivi ->
      begin match args with
        [arg1; Cconst_int n] when n = 1 lsl (Misc.log2 n) ->
          (Iintop_imm(Idiv, n), [arg1])
      | _ ->
          (Iextcall("__divsi3", false), args)
      end
  | Cmodi ->
      begin match args with
        [arg1; Cconst_int n] when n = 1 lsl (Misc.log2 n) ->
          (Iintop_imm(Imod, n), [arg1])
      | _ ->
          (Iextcall("__modsi3", false), args)
      end
  | Ccheckbound ->
      begin match args with
        [Cop(Clsr, [arg1; Cconst_int n]); arg2]
    when n > 0 && n < 32 && not(is_intconst arg2) ->
      (Ispecific(Ishiftcheckbound n), [arg1; arg2])
      | _ ->
        super.select_operation self op args
      end
  | _ -> super.select_operation self op args
 );

(* In mul rd, rm, rs,  rm and rd must be different.
   We deal with this by pretending that rm is also a result of the mul
   operation. *)

 insert_op = (fun self op rs rd ->
  if op = Iintop(Imul) then begin
    self.insert (Iop op) rs [| rd.(0); rs.(0) |]; rd
  end else
    super.insert_op self op rs rd
 );
 }
(*e: function Selection.selector *)


(*s: function Selection.fundecl *)
let fundecl f = 
  let s = selector () in
  s.emit_fundecl s f
(*e: function Selection.fundecl *)

(*e: asmcomp/arm/selection.ml *)
