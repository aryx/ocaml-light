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

(* $Id: coloring.mli,v 1.3 1996/04/30 14:42:22 xleroy Exp $ *)

(* Register allocation by coloring of the interference graph *)

val allocate_registers: unit -> unit
