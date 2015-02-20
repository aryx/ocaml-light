(*s: ./parsing/longident.mli *)
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

(* $Id: longident.mli,v 1.5 1996/04/30 14:49:41 xleroy Exp $ *)

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
