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

(*s: type Syntax.charpos *)
type charpos = int
(*e: type Syntax.charpos *)

(*s: type Syntax.location *)
type location =
    Location of charpos * charpos
(*e: type Syntax.location *)

(*s: type Syntax.char_ *)
type char_ = int
(*e: type Syntax.char_ *)

(*s: type Syntax.regular_expression *)
type regular_expression =
    Epsilon
  | Characters of char_ list
  | Sequence of regular_expression * regular_expression
  | Alternative of regular_expression * regular_expression
  | Repetition of regular_expression
(*e: type Syntax.regular_expression *)

(*s: type Syntax.action *)
type action = location
(*e: type Syntax.action *)

(*s: type Syntax.rule *)
type rule = string * (regular_expression * action) list
(*e: type Syntax.rule *)

(*s: type Syntax.lexer_definition *)
type lexer_definition =
    { header: location;
      entrypoints: rule list;
      trailer: location 
    }
(*e: type Syntax.lexer_definition *)
(*e: lex/syntax.mli *)
