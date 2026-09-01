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

(* Instruction selection for the Motorola 68k *)

open Misc
open Cmm
open Reg
open Arch
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

(* Special constraints on operand and result registers for two-address
   instructions *)

exception Use_default

let pseudoregs_for_operation op arg res =
  match op with
  (* Two-address binary operations *)
    Iintop(Iadd | Isub | Imul | Idiv | Imod | Ilsl | Ilsr | Iasr) |
    Iaddf | Isubf | Imulf | Idivf ->
      ([|res.(0); arg.(1)|], res, false)
  (* Two-address binary operations, forcing the second argument to be
     in a data register *)
  | Iintop(Iand | Ior | Ixor) ->
      let newarg1 = Reg.create Int in
      ([|res.(0); newarg1|], res, false)
  (* Two-address unary operations *)
  | Iintop_imm((Iadd | Isub | Imul | Idiv | Imod | Iand | Ior | Ixor |
                Ilsl | Ilsr | Iasr), _) ->
      (res, res, false)
  (* Other instructions are regular *)
  | _ -> raise Use_default

(* claude: needed by the select_shift override below, to reconstruct a
   Cmm-level shift node for the "shift by 8, then shift by the rest"
   chain (see there). *)
let cmm_op_for_shift = function
    Ilsl -> Clsl
  | Ilsr -> Clsr
  | Iasr -> Casr
  | _ -> fatal_error "Selection_m68k.cmm_op_for_shift"

(* The selector *)

let selector () =
  let super = Selectgen.selector_generic () in
  { super with

  is_immediate = (fun (n : int) -> true);

  (* Select addressing modes *)
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

  select_operation = (fun self op args ->
  match op with
  (* Recognize the LEA instruction *)
    Cadda | Csuba ->
      begin match self.select_addressing (Cop(op, args)) with
        (Iindexed d, _) -> super.select_operation self op args
      | (addr, arg) -> (Ispecific(Ilea addr), [arg])
      end
  (* claude: no case for Clsl/Clsr/Casr here -- Selectgen's generic
     select_operation (reached via the catch-all below) already
     dispatches direct shift operators through self.select_shift, which
     is overridden below to apply m68k's 1-8 immediate-shift-count
     limit. Cmuli DOES need special-casing here, though: the generic
     select_operation's own "multiply by a power of 2" strength
     reduction (Cmuli with a Cconst_int n = 2^l) builds Iintop_imm(Ilsl,
     l) *directly*, bypassing self.select_shift entirely -- so e.g.
     "n * 1024" produced an out-of-range immediate shift "lsll
     #10,%d1", which m68k-linux-gnu-as rejects ("operands mismatch"),
     without ever going through our clamping. Route it through
     self.select_shift ourselves instead. *)
  | Cmuli ->
      begin match args with
        [arg1; Cconst_int n] when n = 1 lsl (Misc.log2 n) ->
          self.select_shift Ilsl [arg1; Cconst_int (Misc.log2 n)]
      | [Cconst_int n; arg1] when n = 1 lsl (Misc.log2 n) ->
          self.select_shift Ilsl [arg1; Cconst_int (Misc.log2 n)]
      | _ -> super.select_operation self op args
      end
  | _ -> super.select_operation self op args
  );

  (* Recognize immediate shifts only if 1 <= count <= 8 *)
  select_shift = (fun iop args ->
  match args with
    [arg1; Cconst_int n] ->
      if n >= 1 && n <= 8 then
        (Iintop_imm(iop, n), [arg1])
      else if n >= 9 && n <= 16 then
        (Iintop_imm(iop, n - 8), [Cop(cmm_op_for_shift iop, [arg1; Cconst_int 8])])
      else
        (Iintop iop, args)
  | args -> (Iintop iop, args)
  );

  (* Select store operations *)
  select_store = (fun addr exp ->
  match exp with
    Cconst_int n -> (Ispecific(Istore_int(n, addr)), Ctuple [])
  | Cconst_pointer n -> (Ispecific(Istore_int(n, addr)), Ctuple [])
  | Cconst_symbol s -> (Ispecific(Istore_symbol(s, addr)), Ctuple [])
  | _ -> super.select_store addr exp
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

  (* Select push operations for external calls *)
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
