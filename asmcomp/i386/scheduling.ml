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

(* $Id: scheduling.ml,v 1.1 1997/07/24 11:49:04 xleroy Exp $ *)

open Schedgen (* to create a dependency *)

(* Scheduling is turned off because our model does not fit the 486
   nor Pentium very well. In particular, it messes up with the
   float reg stack. The Pentium Pro schedules at run-time much better 
   than what we could do. *)

let fundecl f = f
