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

(* $Id: reg.mli,v 1.6 1996/04/30 14:42:56 xleroy Exp $ *)

(* Pseudo-registers *)

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

and location =
    Unknown
  | Reg of int
  | Stack of stack_location

and stack_location =
    Local of int
  | Incoming of int
  | Outgoing of int

val dummy: t
val create: Cmm.machtype_component -> t
val createv: Cmm.machtype -> t array
val clone: t -> t
val at_location: Cmm.machtype_component -> location -> t

val add_set_array: t Set.t -> t array -> t Set.t
val diff_set_array: t Set.t -> t array -> t Set.t
val inter_set_array: t Set.t -> t array -> t Set.t
val set_of_array: t array -> t Set.t

val reset: unit -> unit
val all_registers: unit -> t list
val num_registers: unit -> int
val reinit: unit -> unit
