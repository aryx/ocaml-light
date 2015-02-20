(*s: ./parsing/longident.mli *)
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

(*s: type Longident.t *)
(* Long identifiers, used in parsetree. *)

type t =
    Lident of string
  | Ldot of t * string
(*e: type Longident.t *)

(*s: signature Longident.flatten *)
val flatten: t -> string list
(*e: signature Longident.flatten *)
(*e: ./parsing/longident.mli *)
