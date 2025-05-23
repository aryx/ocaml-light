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


(* Instruction selection for the Intel x86 *)

open Misc
open Arch
open Proc
open Cmm
open Reg
open Mach

open Selectgen

(* Auxiliary for recognizing addressing modes *)

type addressing_expr =
    Asymbol of string
  | Alinear of expression
  | Aadd of expression * expression
  | Ascale of expression * int
  | Ascaledadd of expression * expression * int

let rec select_addr exp =
  match exp with
    Cconst_symbol s ->
      (Asymbol s, 0)
  | Cop((Caddi | Cadda), [arg; Cconst_int m]) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop((Csubi | Csuba), [arg; Cconst_int m]) ->
      let (a, n) = select_addr arg in (a, n - m)
  | Cop((Caddi | Cadda), [Cconst_int m; arg]) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop(Clsl, [arg; Cconst_int(1|2|3 as shift)]) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, 1 lsl shift), n lsl shift)
      | _ -> (Alinear exp, 0)
      end
  | Cop(Cmuli, [arg; Cconst_int(2|4|8 as mult)]) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, mult), n * mult)
      | _ -> (Alinear exp, 0)
      end
  | Cop(Cmuli, [Cconst_int(2|4|8 as mult); arg]) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, mult), n * mult)
      | _ -> (Alinear exp, 0)
      end
  | Cop((Caddi | Cadda), [arg1; arg2]) ->
      begin match (select_addr arg1, select_addr arg2) with
          ((Alinear e1, n1), (Alinear e2, n2)) ->
              (Aadd(e1, e2), n1 + n2)
        | ((Alinear e1, n1), (Ascale(e2, scale), n2)) ->
              (Ascaledadd(e1, e2, scale), n1 + n2)
        | ((Ascale(e1, scale), n1), (Alinear e2, n2)) ->
              (Ascaledadd(e2, e1, scale), n1 + n2)
        | (_, (Ascale(e2, scale), n2)) ->
              (Ascaledadd(arg1, e2, scale), n2)
        | ((Ascale(e1, scale), n1), _) ->
              (Ascaledadd(arg2, e1, scale), n1)
        | _ ->
              (Aadd(arg1, arg2), 0)
      end
  | arg ->
      (Alinear arg, 0)
    
(* Estimate number of float temporaries needed to evaluate expression
   (Ershov's algorithm) *)

let rec float_needs = function
    Cop((Caddf | Csubf | Cmulf | Cdivf), [arg1; arg2]) ->
      let n1 = float_needs arg1 in
      let n2 = float_needs arg2 in
      if n1 = n2 then 1 + n1 else if n1 > n2 then n1 else n2
  | _ ->
      1

(* Special constraints on operand and result registers *)

exception Use_default

let eax = phys_reg 0
let ecx = phys_reg 2
let edx = phys_reg 3
let tos = phys_reg 100

let pseudoregs_for_operation op arg res =
  match op with
  (* Two-address binary operations *)
    Iintop(Iadd|Isub|Imul|Iand|Ior|Ixor) ->
      ([|res.(0); arg.(1)|], res, false)
  (* Two-address unary operations *)
  | Iintop_imm((Iadd|Isub|Imul|Idiv|Iand|Ior|Ixor|Ilsl|Ilsr|Iasr), _) ->
      (res, res, false)
  (* For shifts with variable shift count, second arg must be in ecx *)
  | Iintop(Ilsl|Ilsr|Iasr) ->
      ([|res.(0); ecx|], res, false)
  (* For div and mod, first arg must be in eax, edx is clobbered,
     and result is in eax or edx respectively.
     Keep it simple, just force second argument in ecx. *)
  | Iintop(Idiv) ->
      ([| eax; ecx |], [| eax |], true)
  | Iintop(Imod) ->
      ([| eax; ecx |], [| edx |], true)
  (* For mod with immediate operand, arg must not be in eax.
     Keep it simple, force it in edx. *)
  | Iintop_imm(Imod, _) ->
      ([| edx |], [| edx |], true)
  (* For floating-point operations, the result is always left at the
     top of the floating-point stack *)
  | Iconst_float _ | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Ifloatofint |Ispecific(Isubfrev | Idivfrev | Ifloatarithmem(_, _)) ->
      (arg, [| tos |], false)           (* don't move it immediately *)
  (* Same for a floating-point load *)
  | Iload(Word, addr) when res.(0).typ = Float ->
      (arg, [| tos |], false)
  (* For storing a byte, the argument must be in eax...edx.
     For storing a halfword, any reg is ok.
     Keep it simple, just force it to be in edx in both cases. *)
  | Istore(Word, addr) -> raise Use_default
  | Istore(chunk, addr) ->
      let newarg = Array.copy arg in
      newarg.(0) <- edx;
      (newarg, res, false)
  (* Other instructions are regular *)
  | _ -> raise Use_default

(* The selector class *)

let selector () = 
  let super = Selectgen.selector_generic () in
  {
  instr_seq = dummy_instr;
  (* todo: super with feature needed ... *)
  select_condition = super.select_condition;
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


  is_immediate = (fun (n : int) -> true);

  select_addressing = (fun exp ->
  match select_addr exp with
    (Asymbol s, d) ->
      (Ibased(s, d), Ctuple [])
  | (Alinear e, d) ->
      (Iindexed d, e)
  | (Aadd(e1, e2), d) ->
      (Iindexed2 d, Ctuple[e1; e2])
  | (Ascale(e, scale), d) ->
      (Iscaled(scale, d), e)
  | (Ascaledadd(e1, e2, scale), d) ->
      (Iindexed2scaled(scale, d), Ctuple[e1; e2])
  );

 select_store = (fun addr exp ->
  match exp with
    Cconst_int n -> (Ispecific(Istore_int(n, addr)), Ctuple [])
  | Cconst_pointer n -> (Ispecific(Istore_int(n, addr)), Ctuple [])
  | Cconst_symbol s -> (Ispecific(Istore_symbol(s, addr)), Ctuple [])
  | _ -> super.select_store addr exp
 );

 select_operation = (fun self op args ->
  match op with
  (* Recognize the LEA instruction *)
    Caddi | Cadda | Csubi | Csuba ->
      begin match self.select_addressing (Cop(op, args)) with
        (Iindexed d, _) -> super.select_operation self op args
      | (Iindexed2 0, _) -> super.select_operation self op args
      | (addr, arg) -> (Ispecific(Ilea addr), [arg])
      end
  (* Recognize (x / cst) and (x % cst) only if cst is a power of 2. *)
  | Cdivi ->
      begin match args with
        [arg1; Cconst_int n] when n = 1 lsl (Misc.log2 n) ->
          (Iintop_imm(Idiv, n), [arg1])
      | _ -> (Iintop Idiv, args)
      end
  | Cmodi ->
      begin match args with
        [arg1; Cconst_int n] when n = 1 lsl (Misc.log2 n) ->
          (Iintop_imm(Imod, n), [arg1])
      | _ -> (Iintop Imod, args)
      end
  (* Recognize float arithmetic with memory.
     In passing, apply Ershov's algorithm to reduce stack usage *)
  | Caddf ->
      self.select_floatarith self Iaddf Iaddf Ifloatadd Ifloatadd args
  | Csubf ->
      self.select_floatarith self Isubf (Ispecific Isubfrev) Ifloatsub Ifloatsubrev args
  | Cmulf ->
      self.select_floatarith self Imulf Imulf Ifloatmul Ifloatmul args
  | Cdivf ->
      self.select_floatarith self Idivf (Ispecific Idivfrev) Ifloatdiv Ifloatdivrev args
  (* Recognize store instructions *)
  | Cstore ->
      begin match args with
        [loc; Cop(Caddi, [Cop(Cload _, [loc']); Cconst_int n])]
        when loc = loc' ->
          let (addr, arg) = self.select_addressing loc in
          (Ispecific(Ioffset_loc(n, addr)), [arg])
      | _ ->
          super.select_operation self op args
      end
  | _ -> super.select_operation self op args
 );

(* Recognize float arithmetic with mem *)

 select_floatarith = (fun self regular_op reversed_op mem_op mem_rev_op args ->
  match args with
    [arg1; Cop(Cload _, [loc2])] ->
      let (addr, arg2) = self.select_addressing loc2 in
      (Ispecific(Ifloatarithmem(mem_op, addr)), [arg1; arg2])
  | [Cop(Cload _, [loc1]); arg2] ->
      let (addr, arg1) = self.select_addressing loc1 in
      (Ispecific(Ifloatarithmem(mem_rev_op, addr)), [arg2; arg1])
  | [arg1; arg2] ->
      (* Evaluate bigger subexpression first to minimize stack usage.
         Because of right-to-left evaluation, rightmost arg is evaluated
         first *)
      if float_needs arg1 <= float_needs arg2
      then (regular_op, [arg1; arg2])
      else (reversed_op, [arg2; arg1])
  | _ ->
      fatal_error "Proc_i386: select_floatarith"
 );

(* Deal with register constraints *)

 insert_op = (fun self op rs rd ->
  try
    let (rsrc, rdst, move_res) = pseudoregs_for_operation op rs rd in
    self.insert_moves self rs rsrc;
    self.insert self (Iop op) rsrc rdst;
    if move_res then begin
      self.insert_moves self rdst rd;
      rd
    end else
      rdst
  with Use_default ->
    super.insert_op self op rs rd
 );

(* Selection of push instructions for external calls *)

 select_push = (fun self exp ->
  match exp with
    Cconst_int n -> (Ispecific(Ipush_int n), Ctuple [])
  | Cconst_pointer n -> (Ispecific(Ipush_int n), Ctuple [])
  | Cconst_symbol s -> (Ispecific(Ipush_symbol s), Ctuple [])
  | Cop(Cload ty, [loc]) when ty = typ_float ->
      let (addr, arg) = self.select_addressing loc in
      (Ispecific(Ipush_load_float addr), arg)
  | Cop(Cload ty, [loc]) when ty = typ_addr or ty = typ_int ->
      let (addr, arg) = self.select_addressing loc in
      (Ispecific(Ipush_load addr), arg)
  | _ -> (Ispecific(Ipush), exp)
 );

 emit_extcall_args = (fun self env args ->
  let rec emit_pushes = function
    [] -> 0
  | e :: el ->
      let ofs = emit_pushes el in
      let (op, arg) = self.select_push self e in
      let r = self.emit_expr self env arg in
      self.insert self (Iop op) r [||];
      ofs + Selectgen.size_expr env e
  in ([||], emit_pushes args)
 );
 }


let fundecl f = 
  let s = selector () in
  s.emit_fundecl s f

