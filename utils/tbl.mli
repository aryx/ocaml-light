(*s: utils/tbl.mli *)
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

(* Association tables from any ordered type to any type.
   We use the generic ordering to compare keys. *)

type ('a, 'b) t

(*s: signature [[Tbl.empty]] *)
val empty: ('a, 'b) t
(*e: signature [[Tbl.empty]] *)
(*s: signature [[Tbl.add]] *)
val add: 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
(*e: signature [[Tbl.add]] *)
(*s: signature [[Tbl.find]] *)
val find: 'a -> ('a, 'b) t -> 'b
(*e: signature [[Tbl.find]] *)
(*s: signature [[Tbl.remove]] *)
val remove: 'a -> ('a,  'b) t -> ('a, 'b) t
(*e: signature [[Tbl.remove]] *)
(*s: signature [[Tbl.iter]] *)
val iter: ('a -> 'b -> 'c) -> ('a, 'b) t -> unit
(*e: signature [[Tbl.iter]] *)

(*s: signature [[Tbl.print]] *)
val print: ('a -> unit) -> ('b -> unit) -> ('a, 'b) t -> unit
(*e: signature [[Tbl.print]] *)
(*e: utils/tbl.mli *)
