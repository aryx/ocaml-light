(*s: asmcomp/split.ml *)
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

(* Renaming of registers at reload points to split live ranges. *)

open Reg
open Mach

(*s: type [[Split.subst]] *)
(* Substitutions are represented by register maps *)

type subst = (Reg.t, Reg.t) Map.t
(*e: type [[Split.subst]] *)

(*s: function [[Split.subst_reg]] *)
let subst_reg r sub =
  try
    (*Reg.*)Map.find r sub
  with Not_found ->
    r
(*e: function [[Split.subst_reg]] *)

(*s: function [[Split.subst_regs]] *)
let subst_regs rv sub =
  match sub with
    None -> rv
  | Some s ->
      let n = Array.length rv in
      let nv = Array.create n Reg.dummy in
      for i = 0 to n-1 do nv.(i) <- subst_reg rv.(i) s done;
      nv
(*e: function [[Split.subst_regs]] *)

(*s: constant [[Split.equiv_classes]] *)
(* We maintain equivalence classes of registers using a standard
   union-find algorithm *)

let equiv_classes = ref ((*Reg.*)Map.empty : (Reg.t, Reg.t) Map.t)
(*e: constant [[Split.equiv_classes]] *)

(*s: function [[Split.repres_reg]] *)
let rec repres_reg r =
  try
    repres_reg((*Reg.*)Map.find r !equiv_classes)
  with Not_found ->
    r
(*e: function [[Split.repres_reg]] *)

(*s: function [[Split.repres_regs]] *)
let repres_regs rv =
  let n = Array.length rv in
  for i = 0 to n-1 do rv.(i) <- repres_reg rv.(i) done
(*e: function [[Split.repres_regs]] *)

(*s: function [[Split.identify]] *)
(* Identify two registers.
   The second register is chosen as canonical representative. *)

let identify r1 r2 =
  let repres1 = repres_reg r1 in
  let repres2 = repres_reg r2 in
  if repres1.stamp = repres2.stamp then () else begin
    equiv_classes := (*Reg.*)Map.add repres1 repres2 !equiv_classes
  end
(*e: function [[Split.identify]] *)

(*s: function [[Split.identify_sub]] *)
(* Identify the image of a register by two substitutions.
   Be careful to use the original register as canonical representative
   in case it does not belong to the domain of one of the substitutions. *)

let identify_sub sub1 sub2 reg =
  try
    let r1 = (*Reg.*)Map.find reg sub1 in
    try
      let r2 = (*Reg.*)Map.find reg sub2 in
      identify r1 r2
    with Not_found ->
      identify r1 reg
  with Not_found ->
    try
      let r2 = (*Reg.*)Map.find reg sub2 in
      identify r2 reg
    with Not_found ->
      ()
(*e: function [[Split.identify_sub]] *)

(*s: function [[Split.merge_substs]] *)
(* Identify registers so that the two substitutions agree on the
   registers live before the given instruction. *)

let merge_substs sub1 sub2 i =
  match (sub1, sub2) with
    (None, None) -> None
  | (Some s1, None) -> sub1
  | (None, Some s2) -> sub2
  | (Some s1, Some s2) ->
      (*Reg.*)Set.iter (identify_sub s1 s2) (Reg.add_set_array i.live i.arg);
      sub1
(*e: function [[Split.merge_substs]] *)

(*s: function [[Split.merge_subst_array]] *)
(* Same, for N substitutions *)

let merge_subst_array subv instr =
  let rec find_one_subst i =
    if i >= Array.length subv then None else begin
      match subv.(i) with
        None -> find_one_subst (i+1)
      | Some si as sub ->
          for j = i+1 to Array.length subv - 1 do
            match subv.(j) with
              None -> ()
            | Some sj ->
                (*Reg.*)Set.iter (identify_sub si sj)
                             (Reg.add_set_array instr.live instr.arg)
          done;
          sub
    end in
  find_one_subst 0
(*e: function [[Split.merge_subst_array]] *)

(*s: constant [[Split.exit_subst]] *)
(* First pass: rename registers at reload points *)

let exit_subst = ref (None: subst option)
(*e: constant [[Split.exit_subst]] *)

(*s: function [[Split.rename]] *)
let rec rename i sub =
  match i.desc with
    Iend ->
      (i, sub)
  | Ireturn | Iop(Itailcall_ind) | Iop(Itailcall_imm _) ->
      (instr_cons i.desc (subst_regs i.arg sub) [||] i.next,
       None)
  | Iop Ireload when i.res.(0).loc = Unknown ->
      begin match sub with
        None -> rename i.next sub
      | Some s ->
          let oldr = i.res.(0) in
          let newr = Reg.clone i.res.(0) in
          let (new_next, sub_next) =
            rename i.next (Some((*Reg.*)Map.add oldr newr s)) in
          (instr_cons i.desc i.arg [|newr|] new_next,
           sub_next)
      end
  | Iop _ ->
      let (new_next, sub_next) = rename i.next sub in
      (instr_cons i.desc (subst_regs i.arg sub) (subst_regs i.res sub)
                         new_next,
       sub_next)
  | Iifthenelse(tst, ifso, ifnot) ->
      let (new_ifso, sub_ifso) = rename ifso sub in
      let (new_ifnot, sub_ifnot) = rename ifnot sub in
      let (new_next, sub_next) =
        rename i.next (merge_substs sub_ifso sub_ifnot i.next) in
      (instr_cons (Iifthenelse(tst, new_ifso, new_ifnot))
                  (subst_regs i.arg sub) [||] new_next,
       sub_next)
  | Iswitch(index, cases) ->
      let new_sub_cases = Array.map (fun c -> rename c sub) cases in
      let sub_merge =
        merge_subst_array (Array.map (fun (n, s) -> s) new_sub_cases) i.next in
      let (new_next, sub_next) = rename i.next sub_merge in
      (instr_cons (Iswitch(index, Array.map (fun (n, s) -> n) new_sub_cases))
                  (subst_regs i.arg sub) [||] new_next,
       sub_next)
  | Iloop(body) ->
      let (new_body, sub_body) = rename body sub in
      let (new_next, sub_next) = rename i.next (merge_substs sub sub_body i) in
      (instr_cons (Iloop(new_body)) [||] [||] new_next,
       sub_next)
  | Icatch(body, handler) ->
      let saved_exit_subst = !exit_subst in
      exit_subst := None;
      let (new_body, sub_body) = rename body sub in
      let sub_entry_handler = !exit_subst in
      exit_subst := saved_exit_subst;
      let (new_handler, sub_handler) = rename handler sub_entry_handler in
      let (new_next, sub_next) =
        rename i.next (merge_substs sub_body sub_handler i.next) in
      (instr_cons (Icatch(new_body, new_handler)) [||] [||] new_next,
       sub_next)
  | Iexit ->
      exit_subst := merge_substs !exit_subst sub i;
      (i, None)
  | Itrywith(body, handler) ->
      let (new_body, sub_body) = rename body sub in
      let (new_handler, sub_handler) = rename handler sub in
      let (new_next, sub_next) =
        rename i.next (merge_substs sub_body sub_handler i.next) in
      (instr_cons (Itrywith(new_body, new_handler)) [||] [||] new_next,
       sub_next)
  | Iraise ->
      (instr_cons Iraise (subst_regs i.arg sub) [||] i.next,
       None)
(*e: function [[Split.rename]] *)
      
(*s: function [[Split.set_repres]] *)
(* Second pass: replace registers by their final representatives *)

let set_repres i =
  instr_iter (fun i -> repres_regs i.arg; repres_regs i.res) i
(*e: function [[Split.set_repres]] *)

(*s: function [[Split.fundecl]] *)
(* Entry point *)

let fundecl f =
  equiv_classes := (*Reg.*)Map.empty;
  let new_args = Array.copy f.fun_args in
  let (new_body, sub_body) = rename f.fun_body (Some (*Reg.*)Map.empty) in
  repres_regs new_args;
  set_repres new_body;
  equiv_classes := (*Reg.*)Map.empty;
  { fun_name = f.fun_name;
    fun_args = new_args;
    fun_body = new_body;
    fun_fast = f.fun_fast }
(*e: function [[Split.fundecl]] *)
(*e: asmcomp/split.ml *)
