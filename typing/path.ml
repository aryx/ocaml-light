(*s: ./typing/path.ml *)
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

(*s: type Path.t (./typing/path.ml) *)
(* $Id$ *)

type t =
    Pident of Ident.t
  | Pdot of t * string * int
  | Papply of t * t
(*e: type Path.t (./typing/path.ml) *)

(*s: constant Path.nopos *)
let nopos = -1
(*e: constant Path.nopos *)

(*s: function Path.same *)
let rec same p1 p2 =
  match (p1, p2) with
    (Pident id1, Pident id2) -> Ident.same id1 id2
  | (Pdot(p1, s1, pos1), Pdot(p2, s2, pos2)) -> s1 = s2 & same p1 p2
  | (Papply(fun1, arg1), Papply(fun2, arg2)) -> same fun1 fun2 & same arg1 arg2
  | (_, _) -> false
(*e: function Path.same *)

(*s: function Path.isfree *)
let rec isfree id = function
    Pident id' -> Ident.same id id'
  | Pdot(p, s, pos) -> isfree id p
  | Papply(p1, p2) -> isfree id p1 or isfree id p2
(*e: function Path.isfree *)
(*e: ./typing/path.ml *)
