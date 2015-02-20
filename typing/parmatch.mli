(*s: ./typing/parmatch.mli *)
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

(* $Id: parmatch.mli,v 1.3 1996/04/30 14:53:04 xleroy Exp $ *)

(* Detection of partial matches and unused match cases. *)

open Typedtree

(*s: signature Parmatch.check_partial *)
val check_partial: Location.t -> (pattern * expression) list -> unit
(*e: signature Parmatch.check_partial *)
(*s: signature Parmatch.check_unused *)
val check_unused: (pattern * expression) list -> unit
(*e: signature Parmatch.check_unused *)
(*e: ./typing/parmatch.mli *)
