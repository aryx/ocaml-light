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

(* $Id: reload.ml,v 1.1 1997/07/24 11:49:04 xleroy Exp $ *)

open Cmm
open Arch
open Reg
open Mach

open Reloadgen

(* Reloading for the Intel x86 *)

let stackp r =
  match r.loc with
    Stack _ -> true
  | _ -> false

let reload () =
  let super = Reloadgen.reload_generic () in
  {
  (* todo: super with feature needed ... *)

 fundecl = super.fundecl;
 makeregs = super.makeregs;
 makereg1 = super.makereg1;
 reload = super.reload;


  makereg = (fun r ->
  match r.typ with
    Float -> r
  | _ -> super.makereg r
  );

(* By overriding makereg, we make sure that pseudoregs of type float
   will never be reloaded. Hence there is no need to make special cases for
   floating-point operations. *)

 reload_operation = (fun self op arg res ->
  match op with
    Iintop(Iadd|Isub|Imul|Iand|Ior|Ixor|Icomp _|Icheckbound) ->
      (* One of the two arguments can reside in the stack *)
      if stackp arg.(0) && stackp arg.(1)
      then ([|arg.(0); self.makereg arg.(1)|], res)
      else (arg, res)
  | Iintop(Ilsl|Ilsr|Iasr) | Iintop_imm(_, _) | Ifloatofint | Iintoffloat |
    Ispecific(Ipush) ->
      (* The argument(s) can be either in register or on stack *)
      (arg, res)
  | _ -> (* Other operations: all args and results in registers *)
      super.reload_operation self op arg res
 );

 reload_test = (fun self tst arg ->
  match tst with
    Iinttest cmp ->
      (* One of the two arguments can reside on stack *)
      if stackp arg.(0) && stackp arg.(1)
      then [| self.makereg arg.(0); arg.(1) |]
      else arg
  | _ ->
      (* The argument(s) can be either in register or on stack *)
      arg
 );
 }


let fundecl f =
  let r = reload () in
  r.fundecl r f



