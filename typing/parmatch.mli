(*s: ./typing/parmatch.mli *)
(*s: copyright header0 *)
(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header0 *)

(* Detection of partial matches and unused match cases. *)

open Typedtree

(*s: signature [[Parmatch.check_partial]] *)
val check_partial: 
  Location.t -> (Typedtree.pattern * Typedtree.expression) list -> unit
(*e: signature [[Parmatch.check_partial]] *)
(*s: signature [[Parmatch.check_unused]] *)
val check_unused: 
  (Typedtree.pattern * Typedtree.expression) list -> unit
(*e: signature [[Parmatch.check_unused]] *)
(*e: ./typing/parmatch.mli *)
