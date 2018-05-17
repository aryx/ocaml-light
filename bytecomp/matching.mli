(*s: ./bytecomp/matching.mli *)
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

(* Compilation of pattern-matching *)

open Typedtree
open Lambda

(*s: signature [[Matching.for_function]] *)
val for_function:
        Location.t -> int ref option -> lambda -> (pattern * lambda) list ->
        lambda
(*e: signature [[Matching.for_function]] *)
(*s: signature [[Matching.for_trywith]] *)
val for_trywith:
        lambda -> (pattern * lambda) list -> lambda
(*e: signature [[Matching.for_trywith]] *)
(*s: signature [[Matching.for_let]] *)
val for_let:
        Location.t -> lambda -> pattern -> lambda -> lambda
(*e: signature [[Matching.for_let]] *)
(*s: signature [[Matching.for_multiple_match]] *)
val for_multiple_match:
        Location.t -> lambda list -> (pattern * lambda) list -> lambda
(*e: signature [[Matching.for_multiple_match]] *)
(*s: signature [[Matching.for_tupled_function]] *)
val for_tupled_function:
        Location.t -> Ident.t list -> (pattern list * lambda) list -> lambda
(*e: signature [[Matching.for_tupled_function]] *)

(*s: exception [[Matching.Cannot_flatten]] *)
exception Cannot_flatten
(*e: exception [[Matching.Cannot_flatten]] *)

(*s: signature [[Matching.flatten_pattern]] *)
val flatten_pattern: int -> pattern -> pattern list
(*e: signature [[Matching.flatten_pattern]] *)
(*e: ./bytecomp/matching.mli *)
