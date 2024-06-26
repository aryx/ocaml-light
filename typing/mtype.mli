(*s: typing/mtype.mli *)
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

(* Operations on module types *)

open Types

(*s: signature [[Mtype.scrape]] *)
val scrape: Env.t -> module_type -> module_type
        (* Expand toplevel module type abbreviations
           till hitting a "hard" module type (signature, functor,
           or abstract module type ident. *)
(*e: signature [[Mtype.scrape]] *)
(*s: signature [[Mtype.strengthen]] *)
val strengthen: Env.t -> module_type -> Path.t -> module_type
        (* Strengthen abstract type components relative to the
           given path. *)
(*e: signature [[Mtype.strengthen]] *)
(*s: signature [[Mtype.nondep_supertype]] *)
val nondep_supertype: Env.t -> Ident.t -> module_type -> module_type
        (* Return the smallest supertype of the given type
           in which the given ident does not appear.
           Raise [Not_found] if no such type List.exists. *)
(*e: signature [[Mtype.nondep_supertype]] *)
(*e: typing/mtype.mli *)
