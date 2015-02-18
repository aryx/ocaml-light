(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: trap_barrier.mli,v 1.1 1996/11/29 16:55:09 xleroy Exp $ *)

(************************* Trap barrier ********************************)

val install_trap_barrier : int -> unit

val remove_trap_barrier : unit -> unit

(* Ensure the trap barrier state is up to date in current checkpoint. *)
val update_trap_barrier : unit -> unit

(* Execute `funct' with a trap barrier. *)
(* --- Used by `finish'. *)
val exec_with_trap_barrier : int -> (unit -> unit) -> unit
