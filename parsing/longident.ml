(*s: ./parsing/longident.ml *)
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

(*s: function Longident.flat *)
let rec flat accu = function
    Lident s -> s :: accu
  | Ldot(lid, s) -> flat (s :: accu) lid
(*e: function Longident.flat *)

(*s: function Longident.flatten *)
let flatten lid = flat [] lid
(*e: function Longident.flatten *)
(*e: ./parsing/longident.ml *)
