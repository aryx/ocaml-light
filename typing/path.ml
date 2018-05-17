(*s: typing/path.ml *)
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

(*s: type [[Path.t]] *)
(* Access paths *)

type t =
    Pident of Ident.t
  | Pdot of t *  string * int
(*e: type [[Path.t]] *)

(*s: constant [[Path.nopos]] *)
let nopos = -1
(*e: constant [[Path.nopos]] *)

(*s: function [[Path.same]] *)
let rec same p1 p2 =
  match (p1, p2) with
    (Pident id1, Pident id2) -> Ident.same id1 id2
  | (Pdot(p1, s1, pos1), Pdot(p2, s2, pos2)) -> s1 = s2 & same p1 p2
  | (_, _) -> false
(*e: function [[Path.same]] *)

(*s: function [[Path.isfree]] *)
let rec isfree id = function
    Pident id' -> Ident.same id id'
  | Pdot(p, s, pos) -> isfree id p
(*e: function [[Path.isfree]] *)
(*e: typing/path.ml *)
