(*s: asmcomp/reg.mli *)
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

(* Pseudo-registers *)

(*s: type Reg.t *)
type t =
  { mutable name: string;               (* Name (for printing) *)
    stamp: int;                         (* Unique stamp *)
    typ: Cmm.machtype_component;        (* Type of contents *)
    mutable loc: location;              (* Actual location *)
    mutable spill: bool;                (* "true" to force stack allocation  *)
    mutable interf: t list;             (* Other regs live simultaneously *)
    mutable prefer: (t * int) list;     (* Preferences for other regs *)
    mutable degree: int;                (* Number of other regs live sim. *)
    mutable spill_cost: int;            (* Estimate of spilling cost *)
    mutable visited: bool }             (* For graph walks *)
(*e: type Reg.t *)

(*s: type Reg.location *)
and location =
    Unknown
  | Reg of int
  | Stack of stack_location
(*e: type Reg.location *)

(*s: type Reg.stack_location *)
and stack_location =
    Local of int
  | Incoming of int
  | Outgoing of int
(*e: type Reg.stack_location *)

(*s: signature Reg.dummy *)
val dummy: t
(*e: signature Reg.dummy *)
(*s: signature Reg.create *)
val create: Cmm.machtype_component -> t
(*e: signature Reg.create *)
(*s: signature Reg.createv *)
val createv: Cmm.machtype -> t array
(*e: signature Reg.createv *)
(*s: signature Reg.clone *)
val clone: t -> t
(*e: signature Reg.clone *)
(*s: signature Reg.at_location *)
val at_location: Cmm.machtype_component -> location -> t
(*e: signature Reg.at_location *)

(*s: signature Reg.add_set_array *)
val add_set_array: t Set.t -> t array -> t Set.t
(*e: signature Reg.add_set_array *)
(*s: signature Reg.diff_set_array *)
val diff_set_array: t Set.t -> t array -> t Set.t
(*e: signature Reg.diff_set_array *)
(*s: signature Reg.inter_set_array *)
val inter_set_array: t Set.t -> t array -> t Set.t
(*e: signature Reg.inter_set_array *)
(*s: signature Reg.set_of_array *)
val set_of_array: t array -> t Set.t
(*e: signature Reg.set_of_array *)

(*s: signature Reg.reset *)
val reset: unit -> unit
(*e: signature Reg.reset *)
(*s: signature Reg.all_registers *)
val all_registers: unit -> t list
(*e: signature Reg.all_registers *)
(*s: signature Reg.num_registers *)
val num_registers: unit -> int
(*e: signature Reg.num_registers *)
(*s: signature Reg.reinit *)
val reinit: unit -> unit
(*e: signature Reg.reinit *)
(*e: asmcomp/reg.mli *)
