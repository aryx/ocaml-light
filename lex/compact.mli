(*s: lex/compact.mli *)
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

(*s: type Compact.lex_tables *)
(* Compaction of an automata *)

type lex_tables =
  { tbl_base: int array;                 (* Perform / Shift *)
    tbl_backtrk: int array;              (* No_remember / Remember *)
    tbl_default: int array;              (* Default transition *)
    tbl_trans: int array;                (* Transitions (compacted) *)
    tbl_check: int array }               (* Check (compacted) *)
(*e: type Compact.lex_tables *)

(*s: signature Compact.compact_tables *)
val compact_tables: Lexgen.automata array -> lex_tables
(*e: signature Compact.compact_tables *)
(*e: lex/compact.mli *)
