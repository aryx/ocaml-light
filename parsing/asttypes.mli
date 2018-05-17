(*s: parsing/asttypes.mli *)
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

(* Auxiliary a.s.t. types used by parsetree and typedtree. *)

(*s: type [[Asttypes.constant]] *)
type constant =
    Const_int of int
  | Const_char of char
  | Const_string of string
  | Const_float of string
(*e: type [[Asttypes.constant]] *)

(*s: type [[Asttypes.rec_flag]] *)
type rec_flag = Nonrecursive | Recursive
(*e: type [[Asttypes.rec_flag]] *)

(*s: type [[Asttypes.direction_flag]] *)
type direction_flag = Upto | Downto
(*e: type [[Asttypes.direction_flag]] *)

(*s: type [[Asttypes.mutable_flag]] *)
type mutable_flag = Immutable | Mutable
(*e: type [[Asttypes.mutable_flag]] *)
(*e: parsing/asttypes.mli *)
