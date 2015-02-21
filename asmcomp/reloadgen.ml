(*s: asmcomp/reloadgen.ml *)
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

(* $Id: reloadgen.ml,v 1.2 1997/11/13 10:57:09 xleroy Exp $ *)

(* Insert load/stores for pseudoregs that got assigned to stack locations. *)

open Misc
open Reg
open Mach

(*s: function Reloadgen.insert_move *)
let insert_move src dst next =
  if src.loc = dst.loc
  then next
  else instr_cons (Iop Imove) [|src|] [|dst|] next
(*e: function Reloadgen.insert_move *)

(*s: function Reloadgen.insert_moves *)
let insert_moves src dst next =
  let rec insmoves i =
    if i >= Array.length src
    then next
    else insert_move src.(i) dst.(i) (insmoves (i+1))
  in insmoves 0
(*e: function Reloadgen.insert_moves *)



(*s: type Reloadgen.reloader (asmcomp/reloadgen.ml) *)
type reloader = {
 reload_operation :
    reloader ->
    Mach.operation -> Reg.t array -> Reg.t array -> Reg.t array * Reg.t array;
 reload_test: 
    reloader ->
    Mach.test -> Reg.t array -> Reg.t array;
    (* Can be overriden to reflect instructions that can operate
       directly on stack locations *)
 makereg : Reg.t -> Reg.t;
    (* Can be overriden to avoid creating new registers of some class
       (i.e. if all "registers" of that class are actually on stack) *)
 fundecl : 
   reloader ->
   Mach.fundecl -> Mach.fundecl * bool;
    (* The entry point *)

 (* old: protected *)

 makeregs : reloader -> Reg.t array -> Reg.t array;
 makereg1 : reloader -> Reg.t array -> Reg.t array;
 
 reload:  
   reloader -> Mach.instruction -> Mach.instruction;
 
}
(*e: type Reloadgen.reloader (asmcomp/reloadgen.ml) *)


(*s: function Reloadgen.reload_generic *)
let reload_generic () =
  let redo_regalloc = ref false in
  {
  makereg = (fun r ->
  match r.loc with
    Unknown -> fatal_error "Reload.makereg"
  | Reg _ -> r
  | Stack _ ->
      redo_regalloc := true;
      let newr = Reg.clone r in
      (* Strongly discourage spilling this register *)
      newr.spill_cost <- 100000;
      newr
  );

  makeregs = (fun self rv ->
  let n = Array.length rv in
  let newv = Array.create n Reg.dummy in
  for i = 0 to n-1 do newv.(i) <- self.makereg rv.(i) done;
  newv
  );

  makereg1 = (fun self rv ->
  let newv = Array.copy rv in
  newv.(0) <- self.makereg rv.(0);
  newv
  );

 reload_operation = (fun self op arg res ->
  (* By default, assume that arguments and results must reside
     in hardware registers. For moves, allow one arg or one
     res to be stack-allocated, but do something for
     stack-to-stack moves *)
  match op with
    Imove | Ireload | Ispill ->
      begin match arg.(0), res.(0) with
        {loc = Stack s1}, {loc = Stack s2} when s1 <> s2 ->
          ([| self.makereg arg.(0) |], res)
      | _ ->
          (arg, res)
      end
  | _ ->
      (self.makeregs self arg, self.makeregs self res)
 );

 reload_test = (fun self tst args ->
  self.makeregs self args
 );

 reload = (fun self i ->
  match i.desc with
    (* For function calls, returns, etc: the arguments and results are
       already at the correct position (e.g. on stack for some arguments).
       However, something needs to be done for the function pointer in
       indirect calls. *)
    Iend | Ireturn | Iop(Itailcall_imm _) | Iraise -> i
  | Iop(Itailcall_ind) ->
      let newarg = self.makereg1 self i.arg in
      insert_moves i.arg newarg
        (instr_cons_live i.desc newarg i.res i.live i.next)
  | Iop(Icall_imm _ | Iextcall(_, _)) ->
      instr_cons_live i.desc i.arg i.res i.live (self.reload self i.next)
  | Iop(Icall_ind) ->
      let newarg = self.makereg1 self i.arg in
      insert_moves i.arg newarg
        (instr_cons_live i.desc newarg i.res i.live (self.reload self i.next))
  | Iop op ->
      let (newarg, newres) = self.reload_operation self op i.arg i.res in
      insert_moves i.arg newarg
        (instr_cons_live i.desc newarg newres i.live
          (insert_moves newres i.res
            (self.reload self i.next)))
  | Iifthenelse(tst, ifso, ifnot) ->
      let newarg = self.reload_test self tst i.arg in
      insert_moves i.arg newarg      
        (instr_cons
          (Iifthenelse(tst, self.reload self ifso, self.reload self ifnot)) newarg [||]
          (self.reload self i.next))
  | Iswitch(index, cases) ->
      let newarg = self.makeregs self i.arg in
      insert_moves i.arg newarg      
        (instr_cons (Iswitch(index, Array.map (self.reload self) cases)) newarg [||]
          (self.reload self i.next))
  | Iloop body ->
      instr_cons (Iloop(self.reload self body)) [||] [||] (self.reload self i.next)
  | Icatch(body, handler) ->
      instr_cons (Icatch(self.reload self body, self.reload self handler)) [||] [||]
        (self.reload self i.next)
  | Iexit ->
      instr_cons Iexit [||] [||] dummy_instr
  | Itrywith(body, handler) ->
      instr_cons (Itrywith(self.reload self body, self.reload self handler)) [||] [||]
        (self.reload self i.next)
 );

 fundecl = (fun self f ->
  redo_regalloc := false;
  let new_body = self.reload self f.fun_body in
  ({fun_name = f.fun_name; fun_args = f.fun_args;
    fun_body = new_body; fun_fast = f.fun_fast},
   !redo_regalloc)
 );
 }
(*e: function Reloadgen.reload_generic *)
(*e: asmcomp/reloadgen.ml *)
