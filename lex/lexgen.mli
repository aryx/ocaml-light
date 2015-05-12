(*s: lex/lexgen.mli *)
(*s: copyright ocamllex *)
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
(*e: copyright ocamllex *)
(* Representation of automata *)

(*s: type Lexgen.automata *)
type automata =
    Perform of int
  | Shift of automata_trans * automata_move array
(*e: type Lexgen.automata *)
(*s: type Lexgen.automata_trans *)
and automata_trans =
    No_remember
  | Remember of int
(*e: type Lexgen.automata_trans *)
(*s: type Lexgen.automata_move *)
and automata_move =
    Backtrack
  | Goto of int
(*e: type Lexgen.automata_move *)

(*s: type Lexgen.automata_entry *)
(* Representation of entry points *)

type automata_entry =
  { auto_name: string;
    auto_initial_state: int;
    auto_actions: (int * Syntax.location) list }
(*e: type Lexgen.automata_entry *)

(*s: signature Lexgen.make_dfa *)
(* The entry point *)

val make_dfa: Syntax.lexer_definition -> automata_entry list * automata array
(*e: signature Lexgen.make_dfa *)
(*e: lex/lexgen.mli *)
