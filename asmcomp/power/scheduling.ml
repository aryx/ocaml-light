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

(* Instruction scheduling for the Power PC *)

open Arch
open Mach
open Schedgen

(* claude: the original ocaml sources also overrode reload_retaddr_latency
   (12) / reload_retaddr_issue_cycles (3), tuned so the scheduler places
   enough independent work between the Lreloadretaddr and Lreturn for the
   "blr" branch to fold away -- ocaml-light's Schedgen (based on 1.07,
   before that tuning was added) has no such fields: Lreloadretaddr's
   cost is always estimated via oper_latency/oper_issue_cycles applied to
   Schedgen.some_load (a plain word load), which the two match arms below
   already cover via their own Iload cases, just less precisely than the
   dedicated field would have. *)

let scheduler () =
  let super = Schedgen.scheduler_generic () in
  { super with

  (* Latencies (in cycles). Based roughly on the "common model". *)
  oper_latency = (function
      Ireload -> 2
    | Iload(_, _) -> 2
    | Iconst_float _ -> 2 (* turned into a load *)
    | Iconst_symbol _ -> if toc then 2 (* turned into a load *) else 1
    | Iintop Imul -> 9
    | Iintop_imm(Imul, _) -> 5
    | Iintop(Idiv | Imod) -> 36
    | Iaddf | Isubf -> 4
    | Imulf -> 5
    | Idivf -> 33
    | Ispecific(Imultaddf | Imultsubf) -> 5
    | _ -> 1
  );

  (* Issue cycles.  Rough approximations. *)
  oper_issue_cycles = (function
      Iconst_float _ | Iconst_symbol _ -> if toc then 1 else 2
    | Iload(_, Ibased(_, _)) -> 2
    | Istore(_, Ibased(_, _)) -> 2
    | Ialloc _ -> 4
    | Iintop(Imod) -> 40 (* assuming full stall *)
    | Iintop(Icomp _) -> 4
    | Iintop_imm(Idiv, _) -> 2
    | Iintop_imm(Imod, _) -> 4
    | Iintop_imm(Icomp _, _) -> 4
    | Ifloatofint -> 9
    | Iintoffloat -> 4
    | _ -> 1
  );
  }

let fundecl f =
  let s = scheduler () in
  s.schedule_fundecl s f

