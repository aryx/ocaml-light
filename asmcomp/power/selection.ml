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

(* Instruction selection for the Power PC processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

open Selectgen

(* Recognition of addressing modes *)

type addressing_expr =
    Asymbol of string
  | Alinear of expression
  | Aadd of expression * expression

let rec select_addr = function
    Cconst_symbol s when not toc ->
      (* don't recognize this mode in the TOC-based model *)
      (Asymbol s, 0)
  | Cop((Caddi | Cadda), [arg; Cconst_int m]) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop((Caddi | Cadda), [Cconst_int m; arg]) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop((Caddi | Cadda), [arg1; arg2]) ->
      begin match (select_addr arg1, select_addr arg2) with
          ((Alinear e1, n1), (Alinear e2, n2)) ->
              (Aadd(e1, e2), n1 + n2)
        | _ ->
              (Aadd(arg1, arg2), 0)
      end
  | exp ->
      (Alinear exp, 0)

(* Instruction selection *)

(* claude: lifted out of the old selector class (was called via
   self#select_logical) -- doesn't need self, since it never recurses
   back into the selector. *)
let select_logical op = function
    [arg; Cconst_int n] when n >= 0 & n <= 0xFFFF ->
      (Iintop_imm(op, n), [arg])
  | [Cconst_int n; arg] when n >= 0 & n <= 0xFFFF ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

let selector () =
  let super = Selectgen.selector_generic () in
  { super with

  is_immediate = (fun n -> (n <= 32767) & (n >= -32768));

  select_addressing = (fun exp ->
  match select_addr exp with
    (Asymbol s, d) ->
      (Ibased(s, d), Ctuple [])
  | (Alinear e, d) ->
      (Iindexed d, e)
  | (Aadd(e1, e2), d) ->
      if d = 0
      then (Iindexed2, Ctuple[e1; e2])
      else (Iindexed d, Cop(Cadda, [e1; e2]))
  );

  select_operation = (fun self op args ->
  match (op, args) with
  (* Prevent the recognition of (x / cst) and (x % cst) when cst is not
     a power of 2, which do not correspond to an instruction. *)
    (Cdivi, [arg; Cconst_int n]) when n = 1 lsl (Misc.log2 n) ->
      (Iintop_imm(Idiv, n), [arg])
  | (Cdivi, _) ->
      (Iintop Idiv, args)
  | (Cmodi, [arg; Cconst_int n]) when n = 1 lsl (Misc.log2 n) ->
      (Iintop_imm(Imod, n), [arg])
  | (Cmodi, _) ->
      (Iintop Imod, args)
  (* The and, or and xor instructions have a different range of immediate
     operands than the other instructions *)
  | (Cand, _) -> select_logical Iand args
  | (Cor, _) -> select_logical Ior args
  | (Cxor, _) -> select_logical Ixor args
  (* intoffloat goes through a library function on the RS6000 *)
  | (Cintoffloat, _) when not powerpc ->
      (Iextcall("itrunc", false), args)
  (* Recognize mult-add and mult-sub instructions *)
  | (Caddf, [Cop(Cmulf, [arg1; arg2]); arg3]) ->
      (Ispecific Imultaddf, [arg1; arg2; arg3])
  | (Caddf, [arg3; Cop(Cmulf, [arg1; arg2])]) ->
      (Ispecific Imultaddf, [arg1; arg2; arg3])
  | (Csubf, [Cop(Cmulf, [arg1; arg2]); arg3]) ->
      (Ispecific Imultsubf, [arg1; arg2; arg3])
  | _ ->
      super.select_operation self op args
  );
  }

let fundecl f =
  let s = selector () in
  s.emit_fundecl s f
