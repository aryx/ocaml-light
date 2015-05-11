(*s: lex/output.mli *)
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

(*s: signature Output.output_lexdef *)
(* Output the DFA tables and its entry points *)

val output_lexdef:
      in_channel -> out_channel ->
      Syntax.location ->
      Compact.lex_tables ->
      Lexgen.automata_entry list ->
      Syntax.location ->
      unit
(*e: signature Output.output_lexdef *)
(*e: lex/output.mli *)
