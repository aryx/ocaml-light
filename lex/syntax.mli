(*s: lex/syntax.mli *)
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

(*s: type Syntax.location *)
(* The shallow abstract syntax *)

type location =
    Location of int * int
(*e: type Syntax.location *)

(*s: type Syntax.regular_expression *)
type regular_expression =
    Epsilon
  | Characters of int list
  | Sequence of regular_expression * regular_expression
  | Alternative of regular_expression * regular_expression
  | Repetition of regular_expression
(*e: type Syntax.regular_expression *)

(*s: type Syntax.lexer_definition *)
type lexer_definition =
    { header: location;
      entrypoints: (string * (regular_expression * location) list) list;
      trailer: location }
(*e: type Syntax.lexer_definition *)
(*e: lex/syntax.mli *)
