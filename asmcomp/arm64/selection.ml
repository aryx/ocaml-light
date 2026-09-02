(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                  Benedikt Meurer, University of Siegen              *)
(*                                                                     *)
(*    Copyright 2013 Institut National de Recherche en Informatique    *)
(*    et en Automatique. Copyright 2012 Benedikt Meurer. All rights    *)
(*    reserved.  This file is distributed  under the terms of the Q    *)
(*    Public License version 1.0.                                      *)
(*                                                                     *)
(***********************************************************************)

(* Instruction selection for the ARM processor, 64-bit mode *)

open Arch
open Cmm
open Mach

open Selectgen

(* claude: upstream 4.02's select_addressing takes an extra Cmm.memory_chunk
   parameter (is_offset below is chunk-scaled: word/double accesses get a
   12-bit *scaled* immediate, bytes get a wider unscaled range, etc.) --
   but this fork's Selectgen.selector record has no room for it
   (select_addressing : Cmm.expression -> Arch.addressing_mode *
   Cmm.expression, no self, no chunk; see selectgen.mli). Since we can't
   know the chunk here, accept either the conservative *unscaled* 9-bit
   signed offset (-256..255, valid for every access size via LDUR/STUR
   and their b/h/sb/sh variants) or a non-negative multiple of 8 up to
   32760 (the scaled 12-bit range for 8-byte Word/Double accesses via
   LDR/STR) -- emit.mlp picks whichever mnemonic family the actual
   offset needs. The latter matters in practice: Selectgen.emit_stores
   (e.g. for an array or record literal with more than ~30 fields, as in
   stdlib/random.ml's Lagged Fibonacci state table) walks a single
   chosen addressing mode across many fields via Arch.offset_addressing
   without ever re-checking is_offset, so the unscaled range alone is
   exhausted well before such a literal's last field. Chunks other than
   Word/Double with a large *and* non-8-aligned offset are not covered by
   this (would need the chunk to pick the right scale) and fall back to
   Iindexed 0 below instead -- not observed in practice since those
   chunks only ever arise from register-computed offsets, never a large
   compile-time-constant one. *)
let is_offset n = (n >= -256 && n <= 255) || (n >= 0 && n <= 32760 && n land 7 = 0)

let is_intconst = function
    Cconst_int _ -> true
  | _ -> false

(* Immediate operands are a 12-bit unsigned value, optionally shifted
   left by 12 (the ADD/SUB immediate encoding); allow negating too since
   Caddi/Csubi below turn a negative immediate into the other operation. *)

let is_immediate n =
  let mn = -n in
  n land 0xFFF = n || n land 0xFFF000 = n
  || mn land 0xFFF = mn || mn land 0xFFF000 = mn

(* An automaton to recognize ( 0+1+0* | 1+0+1* )

               0          1          0
              / \        / \        / \
              \ /        \ /        \ /
        -0--> [1] --1--> [2] --0--> [3]
       /
     [0]
       \
        -1--> [4] --0--> [5] --1--> [6]
              / \        / \        / \
              \ /        \ /        \ /
               1          0          1

The accepting states are 2, 3, 5 and 6. *)

let auto_table = [|   (* accepting?, next on 0, next on 1 *)
  (* state 0 *) (false, 1, 4);
  (* state 1 *) (false, 1, 2);
  (* state 2 *) (true,  3, 2);
  (* state 3 *) (true,  3, 7);
  (* state 4 *) (false, 5, 4);
  (* state 5 *) (true,  5, 6);
  (* state 6 *) (true,  7, 6);
  (* state 7 *) (false, 7, 7)   (* error state *)
|]

let rec run_automata nbits state input =
  let (acc, next0, next1) = auto_table.(state) in
  if nbits <= 0
  then acc
  else run_automata (nbits - 1)
                    (if input land 1 = 0 then next0 else next1)
                    (input asr 1)

(* We are very conservative wrt what ARM64 supports: we don't support
   repetitions of a 000111000 or 1110000111 pattern, just a single
   pattern of this kind. *)

let is_logical_immediate n =
  n <> 0 && n <> -1 && run_automata 64 0 n

(* Instruction selection *)

let selector () =
  let super = Selectgen.selector_generic () in

  let select_logical op = function
    | [arg; Cconst_int n] when is_logical_immediate n ->
        (Iintop_imm(op, n), [arg])
    | [Cconst_int n; arg] when is_logical_immediate n ->
        (Iintop_imm(op, n), [arg])
    | args ->
        (Iintop op, args)
  in

  { super with

  is_immediate = is_immediate;

  select_addressing = (function
    | Cop(Cadda, [Cconst_symbol s; Cconst_int n]) ->
        (Ibased(s, n), Ctuple [])
    | Cop(Cadda, [arg; Cconst_int n]) when is_offset n ->
        (Iindexed n, arg)
    | Cop(Cadda, [arg1; Cop(Caddi, [arg2; Cconst_int n])]) when is_offset n ->
        (Iindexed n, Cop(Cadda, [arg1; arg2]))
    | Cconst_symbol s ->
        (Ibased(s, 0), Ctuple [])
    | arg ->
        (Iindexed 0, arg)
  );

  select_operation = (fun self op args ->
   match op with
   (* Integer addition *)
   | Caddi | Cadda ->
       begin match args with
       (* Add immediate *)
       | [arg; Cconst_int n] when is_immediate n ->
           ((if n >= 0 then Iintop_imm(Iadd, n) else Iintop_imm(Isub, -n)),
            [arg])
       | [Cconst_int n; arg] when is_immediate n ->
           ((if n >= 0 then Iintop_imm(Iadd, n) else Iintop_imm(Isub, -n)),
            [arg])
       (* Shift-add *)
       | [arg1; Cop(Clsl, [arg2; Cconst_int n])] when n > 0 && n < 64 ->
           (Ispecific(Ishiftarith(Ishiftadd, n)), [arg1; arg2])
       | [arg1; Cop(Casr, [arg2; Cconst_int n])] when n > 0 && n < 64 ->
           (Ispecific(Ishiftarith(Ishiftadd, -n)), [arg1; arg2])
       | [Cop(Clsl, [arg1; Cconst_int n]); arg2] when n > 0 && n < 64 ->
           (Ispecific(Ishiftarith(Ishiftadd, n)), [arg2; arg1])
       | [Cop(Casr, [arg1; Cconst_int n]); arg2] when n > 0 && n < 64 ->
           (Ispecific(Ishiftarith(Ishiftadd, -n)), [arg2; arg1])
       (* Multiply-add *)
       | [arg1; Cop(Cmuli, args2)] ->
           begin match self.select_operation self Cmuli args2 with
           | (Iintop_imm(Ilsl, l), [arg3]) ->
               (Ispecific(Ishiftarith(Ishiftadd, l)), [arg1; arg3])
           | (Iintop Imul, [arg3; arg4]) ->
               (Ispecific Imuladd, [arg3; arg4; arg1])
           | _ ->
               super.select_operation self op args
           end
       | [Cop(Cmuli, args2); arg1] ->
           begin match self.select_operation self Cmuli args2 with
           | (Iintop_imm(Ilsl, l), [arg3]) ->
               (Ispecific(Ishiftarith(Ishiftadd, l)), [arg1; arg3])
           | (Iintop Imul, [arg3; arg4]) ->
               (Ispecific Imuladd, [arg3; arg4; arg1])
           | _ ->
               super.select_operation self op args
           end
       | _ ->
           super.select_operation self op args
       end
   (* Integer subtraction *)
   | Csubi | Csuba ->
       begin match args with
       (* Sub immediate *)
       | [arg; Cconst_int n] when is_immediate n ->
           ((if n >= 0 then Iintop_imm(Isub, n) else Iintop_imm(Iadd, -n)),
            [arg])
       (* Shift-sub *)
       | [arg1; Cop(Clsl, [arg2; Cconst_int n])] when n > 0 && n < 64 ->
           (Ispecific(Ishiftarith(Ishiftsub, n)), [arg1; arg2])
       | [arg1; Cop(Casr, [arg2; Cconst_int n])] when n > 0 && n < 64 ->
           (Ispecific(Ishiftarith(Ishiftsub, -n)), [arg1; arg2])
       (* Multiply-sub *)
       | [arg1; Cop(Cmuli, args2)] ->
           begin match self.select_operation self Cmuli args2 with
           | (Iintop_imm(Ilsl, l), [arg3]) ->
               (Ispecific(Ishiftarith(Ishiftsub, l)), [arg1; arg3])
           | (Iintop Imul, [arg3; arg4]) ->
               (Ispecific Imulsub, [arg3; arg4; arg1])
           | _ ->
               super.select_operation self op args
           end
       | _ ->
           super.select_operation self op args
       end
   (* Checkbounds *)
   | Ccheckbound ->
       begin match args with
       | [Cop(Clsr, [arg1; Cconst_int n]); arg2] when n > 0 && n < 64 ->
           (Ispecific(Ishiftcheckbound n), [arg1; arg2])
       | _ ->
           super.select_operation self op args
       end
   (* Integer multiplication *)
   (* ARM does not support immediate operands for multiplication *)
   | Cmuli ->
       begin match args with
       | [arg; Cconst_int n] ->
           let l = Misc.log2 n in
           if n = 1 lsl l
           then (Iintop_imm(Ilsl, l), [arg])
           else (Iintop Imul, args)
       | [Cconst_int n; arg] ->
           let l = Misc.log2 n in
           if n = 1 lsl l
           then (Iintop_imm(Ilsl, l), [arg])
           else (Iintop Imul, args)
       | _ ->
           (Iintop Imul, args)
       end
   (* Division and modulus *)
   (* Recognize (x / cst) and (x % cst) only if cst is a power of 2. *)
   | Cdivi ->
       begin match args with
       | [arg; Cconst_int n] when n = 1 lsl Misc.log2 n ->
           ((if n = 1 then Imove else Iintop_imm(Idiv, n)), [arg])
       | _ ->
           (Iintop Idiv, args)
       end
   | Cmodi ->
       begin match args with
       | [arg; Cconst_int n] when n = 1 lsl Misc.log2 n ->
           ((if n = 1 then Iconst_int (Nativeint.from 0)
             else Iintop_imm(Imod, n)), [arg])
       | _ ->
           (Iintop Imod, args)
       end
   (* Bitwise logical operations have a different range of immediate
      operands than the other instructions *)
   | Cand -> select_logical Iand args
   | Cor -> select_logical Ior args
   | Cxor -> select_logical Ixor args
   (* Recognize floating-point negate and multiply *)
   | Cnegf ->
       begin match args with
       | [Cop(Cmulf, args)] -> (Ispecific Inegmulf, args)
       | _ -> super.select_operation self op args
       end
   (* Recognize floating-point multiply and add/sub *)
   | Caddf ->
       begin match args with
       | [arg; Cop(Cmulf, args)] ->
           (Ispecific Imuladdf, arg :: args)
       | [Cop(Cmulf, args); arg] ->
           (Ispecific Imuladdf, arg :: args)
       | _ ->
           super.select_operation self op args
       end
   | Csubf ->
       begin match args with
       | [arg; Cop(Cmulf, args)] ->
           (Ispecific Imulsubf, arg :: args)
       | [Cop(Cmulf, args); arg] ->
           (Ispecific Inegmulsubf, arg :: args)
       | _ ->
           super.select_operation self op args
       end
   (* Recognize floating-point square root *)
   | Cextcall("sqrt", _, _) ->
       (Ispecific Isqrtf, args)
   (* Other operations are regular *)
   | _ ->
       super.select_operation self op args
  );
  }

let fundecl f =
  let s = selector () in
  s.emit_fundecl s f
