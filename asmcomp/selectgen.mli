(*s: asmcomp/selectgen.mli *)
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

(*s: type [[Selectgen.environment]] *)
(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

type environment = (Ident.t, Reg.t array) Tbl.t
(*e: type [[Selectgen.environment]] *)

(*s: signature [[Selectgen.size_expr]] *)
val size_expr : environment -> Cmm.expression -> int
(*e: signature [[Selectgen.size_expr]] *)


(*s: type [[Selectgen.selector]] *)
type selector = {
  (* old: nested private field *)
  mutable instr_seq: Mach.instruction;
    
  (* old: virtuals *)

  (* The following methods must or can be overriden by the processor
     description *)
  is_immediate : (int -> bool);
    (* Must be defined to indicate whether a constant is a suitable
       immediate operand to arithmetic instructions *)
  select_addressing :
    (Cmm.expression -> Arch.addressing_mode * Cmm.expression);
    (* Must be defined to select addressing modes *)

  (* old: regular *)

  select_operation :
    selector ->
    Cmm.operation ->
    Cmm.expression list -> Mach.operation * Cmm.expression list;
    (* Can be overriden to deal with special arithmetic instructions *)
  select_condition : 
    selector ->
    Cmm.expression -> Mach.test * Cmm.expression;
    (* Can be overriden to deal with special test instructions *)
  select_store :
    Arch.addressing_mode -> Cmm.expression -> Mach.operation * Cmm.expression;
    (* Can be overriden to deal with special store constant instructions *)
  insert_op :
    selector ->
    Mach.operation -> Reg.t array -> Reg.t array -> Reg.t array;
    (* Can be overriden to deal with 2-address instructions
       or instructions with hardwired input/output registers *)
  emit_extcall_args :
    selector ->
    environment -> Cmm.expression list -> Reg.t array * int;
    (* Can be overriden to deal with stack-based calling conventions *)

  (* The following method is the entry point and should not be overriden *)
  emit_fundecl : 
    selector ->
    Cmm.fundecl -> Mach.fundecl;
  
  (* The following methods should not be overriden.  They cannot be
     declared "private" in the current implementation because they
     are not always applied to "self", but ideally they should be private. *)
  extract : selector -> Mach.instruction;
  insert :
    selector ->
    Mach.instruction_desc -> Reg.t array -> Reg.t array -> unit;
  insert_move : 
    selector ->
    Reg.t -> Reg.t -> unit;
  insert_move_args : 
    selector ->
    Reg.t array -> Reg.t array -> int -> unit;
  insert_move_results : 
    selector ->
    Reg.t array -> Reg.t array -> int -> unit;
  insert_moves : 
    selector ->
    Reg.t array -> Reg.t array -> unit;
  emit_expr :
    selector ->
    (Ident.t, Reg.t array) Tbl.t -> Cmm.expression -> Reg.t array;

  emit_tail : 
    selector ->
    (Ident.t, Reg.t array) Tbl.t -> Cmm.expression -> unit;


  (* old: protected *)

  select_arith_comm: 
     selector -> 
     Mach.integer_operation -> Cmm.expression list -> 
     Mach.operation * Cmm.expression list;
  select_arith: 
     selector -> 
     Mach.integer_operation -> Cmm.expression list -> 
     Mach.operation * Cmm.expression list;
  select_shift: 
     Mach.integer_operation -> Cmm.expression list -> 
     Mach.operation * Cmm.expression list;
  select_arith_comp: 
     selector -> 
     Mach.integer_comparison -> Cmm.expression list -> 
     Mach.operation * Cmm.expression list;


  emit_let :
    selector ->
    (Ident.t, Reg.t array) Tbl.t -> Ident.t -> Cmm.expression -> 
     (Ident.t, Reg.t array) Tbl.t;
  emit_parts_list:
    selector ->
    (Ident.t, Reg.t array) Tbl.t -> Cmm.expression list -> 
     Cmm.expression list * (Ident.t, Reg.t array) Tbl.t;
  emit_parts:
    selector ->
    (Ident.t, Reg.t array) Tbl.t -> Cmm.expression -> 
     Cmm.expression * (Ident.t, Reg.t array) Tbl.t;
  emit_tuple:
    selector ->
    (Ident.t, Reg.t array) Tbl.t -> Cmm.expression list -> 
     Reg.t array;
  emit_stores: 
    selector ->
    (Ident.t, Reg.t array) Tbl.t -> Cmm.expression list -> Reg.t array -> 
     Arch.addressing_mode -> unit;
  emit_sequence:
    selector ->
    (Ident.t, Reg.t array) Tbl.t -> Cmm.expression -> 
     Reg.t array * selector;

  emit_return: 
    selector ->
    (Ident.t, Reg.t array) Tbl.t -> Cmm.expression -> 
     unit;

  emit_tail_sequence: 
    selector ->
    (Ident.t, Reg.t array) Tbl.t -> Cmm.expression -> 
    Mach.instruction;

  (* old: in children *)

  select_floatarith: 
   selector ->
   Mach.operation -> 
   Mach.operation -> 
   Arch.float_operation -> 
   Arch.float_operation -> 
   Cmm.expression list -> 
   Mach.operation * Cmm.expression list;

  select_push:
   selector ->
   Cmm.expression -> Mach.operation * Cmm.expression;

}
(*e: type [[Selectgen.selector]] *)

(*s: signature [[Selectgen.selector_generic]] *)
val selector_generic: unit -> selector
(*e: signature [[Selectgen.selector_generic]] *)


(*
  is_immediate = super.is_immediate;
  select_addressing = super.select_addressing;
  select_operation = super.select_operation;
  select_condition = super.select_condition;
  select_store = super.select_store;
  insert_op = super.insert_op;
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
*)
(*e: asmcomp/selectgen.mli *)
