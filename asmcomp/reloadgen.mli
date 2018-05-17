(*s: asmcomp/reloadgen.mli *)
(*s: copyright header 1997 *)
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
(*e: copyright header 1997 *)

(*s: type [[Reloadgen.reloader]] *)

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
(*e: type [[Reloadgen.reloader]] *)

(*s: signature [[Reloadgen.reload_generic]] *)
val reload_generic: unit -> reloader
(*e: signature [[Reloadgen.reload_generic]] *)

(*
 reload_operation = super.eload_operation;
 reload_test: = super.eload_test:; makereg : Reg.t -> Reg.t;
 fundecl = super.undecl;
 makeregs = super.akeregs;
 makereg1 = super.akereg1;
 reload: = super.eload:;
*)
(*e: asmcomp/reloadgen.mli *)
