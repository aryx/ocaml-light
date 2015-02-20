(*s: ./typing/btype.mli *)
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

(* $Id: btype.mli,v 1.2 1997/05/11 21:48:16 vouillon Exp $ *)

(* Basic operations on core types *)

open Types

(*s: signature Btype.generic_level *)
val generic_level: int
(*e: signature Btype.generic_level *)

(*s: signature Btype.newgenty *)
val newgenty: type_desc -> type_expr
        (* Create a generic type *)
(*e: signature Btype.newgenty *)
(*s: signature Btype.newgenvar *)
val newgenvar: unit -> type_expr
        (* Return a fresh generic variable *)
(*e: signature Btype.newgenvar *)
(*s: signature Btype.newmarkedgenvar *)
val newmarkedgenvar: unit -> type_expr
        (* Return a fresh marked generic variable *)
(*e: signature Btype.newmarkedgenvar *)

(*s: signature Btype.repr *)
val repr: type_expr -> type_expr
        (* Return the canonical representative of a type. *)
(*e: signature Btype.repr *)

(*s: signature Btype.iter_type_expr *)
(**** Utilities for type traversal ****)

val iter_type_expr: (type_expr -> unit) -> type_expr -> unit
        (* Iteration on types *)
(*e: signature Btype.iter_type_expr *)

(*s: signature Btype.save_desc *)
val save_desc: type_expr -> type_desc -> unit
        (* Save a type description *)
(*e: signature Btype.save_desc *)
(*s: signature Btype.cleanup_types *)
val cleanup_types: unit -> unit
        (* Restore type descriptions *)
(*e: signature Btype.cleanup_types *)

(*s: signature Btype.lowest_level *)
val lowest_level: int
        (* Marked type: ty.level < lowest_level *)
(*e: signature Btype.lowest_level *)
(*s: signature Btype.pivot_level *)
val pivot_level: int
        (* Type marking: ty.level <- pivot_level - ty.level *)
(*e: signature Btype.pivot_level *)
(*s: signature Btype.unmark_type *)
val unmark_type: type_expr -> unit
        (* Remove marks from a type *)
(*e: signature Btype.unmark_type *)

(*s: signature Btype.cleanup_abbrev *)
(**** Memorization of abbreviation expansion ****)

val cleanup_abbrev: unit -> unit
        (* Flush the cache of abbreviation expansions.
           When some types are saved (using [output_value]), this
           function MUST be called just before. *)
(*e: signature Btype.cleanup_abbrev *)
(*s: signature Btype.memorize_abbrev *)
val memorize_abbrev:
        abbrev_memo ref -> Path.t -> type_expr -> unit
        (* Add an expansion in the cache *)
(*e: signature Btype.memorize_abbrev *)
(*e: ./typing/btype.mli *)
