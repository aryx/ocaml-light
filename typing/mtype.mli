(*s: ./typing/mtype.mli *)
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

(* Operations on module types *)

open Types

(*s: signature Mtype.scrape *)
val scrape: Env.t -> module_type -> module_type
        (* Expand toplevel module type abbreviations
           till hitting a "hard" module type (signature, functor,
           or abstract module type ident. *)
(*e: signature Mtype.scrape *)
(*s: signature Mtype.strengthen *)
val strengthen: Env.t -> module_type -> Path.t -> module_type
        (* Strengthen abstract type components relative to the
           given path. *)
(*e: signature Mtype.strengthen *)
(*e: ./typing/mtype.mli *)
