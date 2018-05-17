(*s: asmcomp/proc.mli *)
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

(* Processor descriptions *)

(*s: signature [[Proc.word_addressed]] *)
(* Instruction selection *)
val word_addressed: bool
(*e: signature [[Proc.word_addressed]] *)

(*s: signature [[Proc.num_register_classes]] *)
(* Registers available for register allocation *)
val num_register_classes: int
(*e: signature [[Proc.num_register_classes]] *)
(*s: signature [[Proc.register_class]] *)
val register_class: Reg.t -> int
(*e: signature [[Proc.register_class]] *)
(*s: signature [[Proc.num_available_registers]] *)
val num_available_registers: int array
(*e: signature [[Proc.num_available_registers]] *)
(*s: signature [[Proc.first_available_register]] *)
val first_available_register: int array
(*e: signature [[Proc.first_available_register]] *)
(*s: signature [[Proc.register_name]] *)
val register_name: int -> string
(*e: signature [[Proc.register_name]] *)
(*s: signature [[Proc.phys_reg]] *)
val phys_reg: int -> Reg.t
(*e: signature [[Proc.phys_reg]] *)
(*s: signature [[Proc.rotate_registers]] *)
val rotate_registers: bool
(*e: signature [[Proc.rotate_registers]] *)

(*s: signature [[Proc.loc_arguments]] *)
(* Calling conventions *)
val loc_arguments: Reg.t array -> Reg.t array * int
(*e: signature [[Proc.loc_arguments]] *)
(*s: signature [[Proc.loc_results]] *)
val loc_results: Reg.t array -> Reg.t array
(*e: signature [[Proc.loc_results]] *)
(*s: signature [[Proc.loc_parameters]] *)
val loc_parameters: Reg.t array -> Reg.t array
(*e: signature [[Proc.loc_parameters]] *)
(*s: signature [[Proc.loc_external_arguments]] *)
val loc_external_arguments: Reg.t array -> Reg.t array * int
(*e: signature [[Proc.loc_external_arguments]] *)
(*s: signature [[Proc.loc_external_results]] *)
val loc_external_results: Reg.t array -> Reg.t array
(*e: signature [[Proc.loc_external_results]] *)
(*s: signature [[Proc.loc_exn_bucket]] *)
val loc_exn_bucket: Reg.t
(*e: signature [[Proc.loc_exn_bucket]] *)

(*s: signature [[Proc.safe_register_pressure]] *)
(* Maximal register pressures for pre-spilling *)
val safe_register_pressure: Mach.operation -> int
(*e: signature [[Proc.safe_register_pressure]] *)
(*s: signature [[Proc.max_register_pressure]] *)
val max_register_pressure: Mach.operation -> int array
(*e: signature [[Proc.max_register_pressure]] *)

(*s: signature [[Proc.destroyed_at_oper]] *)
(* Registers destroyed by operations *)
val destroyed_at_oper: Mach.instruction_desc -> Reg.t array
(*e: signature [[Proc.destroyed_at_oper]] *)
(*s: signature [[Proc.destroyed_at_raise]] *)
val destroyed_at_raise: Reg.t array
(*e: signature [[Proc.destroyed_at_raise]] *)

(*s: signature [[Proc.num_stack_slots]] *)
(* Info for laying out the stack frame *)
val num_stack_slots: int array
(*e: signature [[Proc.num_stack_slots]] *)
(*s: signature [[Proc.contains_calls]] *)
val contains_calls: bool ref
(*e: signature [[Proc.contains_calls]] *)

(*s: signature [[Proc.assemble_file]] *)
(* Calling the assembler *)
val assemble_file: string -> string -> int
(*e: signature [[Proc.assemble_file]] *)
(*e: asmcomp/proc.mli *)
