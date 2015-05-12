(*s: lex/syntax.mli *)
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
(* The shallow abstract syntax *)

(*s: type Syntax.location *)
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
      trailer: location 
    }
(*e: type Syntax.lexer_definition *)
(*e: lex/syntax.mli *)
