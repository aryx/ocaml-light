(*s: ./typing/path.ml *)
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

(*s: type Path.t (./typing/path.ml) *)
(* $Id: path.ml,v 1.5 1996/07/15 16:35:34 xleroy Exp $ *)

type t =
    Pident of Ident.t
  | Pdot of t * string * int
(*e: type Path.t (./typing/path.ml) *)

(*s: constant Path.nopos *)
let nopos = -1
(*e: constant Path.nopos *)

(*s: function Path.same *)
let rec same p1 p2 =
  match (p1, p2) with
    (Pident id1, Pident id2) -> Ident.same id1 id2
  | (Pdot(p1, s1, pos1), Pdot(p2, s2, pos2)) -> s1 = s2 & same p1 p2
  | (_, _) -> false
(*e: function Path.same *)

(*s: function Path.isfree *)
let rec isfree id = function
    Pident id' -> Ident.same id id'
  | Pdot(p, s, pos) -> isfree id p
(*e: function Path.isfree *)

(*s: constant Path.binding_time *)
let rec binding_time = function
    Pident id -> Ident.binding_time id
  | Pdot(p, s, pos) -> binding_time p
(*e: constant Path.binding_time *)
(*e: ./typing/path.ml *)
