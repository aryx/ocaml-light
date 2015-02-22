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

(* Detection of partial matches and unused match cases. *)

open Typedtree

val check_partial: 
      Location.t -> (Typedtree.pattern * Typedtree.expression) list -> unit
val check_unused: (Typedtree.pattern * Typedtree.expression) list -> unit
