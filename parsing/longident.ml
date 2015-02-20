(*s: ./parsing/longident.ml *)
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

(*s: type Longident.t (./parsing/longident.ml) *)
(* $Id: longident.ml,v 1.2 1996/04/30 14:49:40 xleroy Exp $ *)

type t =
    Lident of string
  | Ldot of t * string
(*e: type Longident.t (./parsing/longident.ml) *)

(*s: function Longident.flat *)
let rec flat accu = function
    Lident s -> s :: accu
  | Ldot(lid, s) -> flat (s :: accu) lid
(*e: function Longident.flat *)

(*s: function Longident.flatten *)
let flatten lid = flat [] lid
(*e: function Longident.flatten *)
(*e: ./parsing/longident.ml *)
