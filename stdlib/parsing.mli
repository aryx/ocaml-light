(*s: stdlib/parsing.mli *)
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

(*s: signature Parsing.symbol_start (yacc) *)
(* Module [Parsing]: the run-time library for parsers generated by [ocamlyacc]*)

val symbol_start : unit -> int
(*e: signature Parsing.symbol_start (yacc) *)
(*s: signature Parsing.symbol_end (yacc) *)
val symbol_end : unit -> int
        (* [symbol_start] and [symbol_end] are to be called in the action part
           of a grammar rule only. They return the position of the string that
           matches the left-hand side of the rule: [symbol_start()] returns
           the position of the first character; [symbol_end()] returns the
           position of the last character, plus one. The first character
           in a file is at position 0. *)
(*e: signature Parsing.symbol_end (yacc) *)
(*s: signature Parsing.rhs_start (yacc) *)
val rhs_start: int -> int
(*e: signature Parsing.rhs_start (yacc) *)
(*s: signature Parsing.rhs_end (yacc) *)
val rhs_end: int -> int
        (* Same as [symbol_start] and [symbol_end], but return the
           position of the string matching the [n]th item on the
           right-hand side of the rule, where [n] is the integer parameter
           to [lhs_start] and [lhs_end]. [n] is 1 for the leftmost item. *)
(*e: signature Parsing.rhs_end (yacc) *)
(*s: signature Parsing.clear_parser (yacc) *)
val clear_parser : unit -> unit
        (* Empty the parser stack. Call it just after a parsing function
           has returned, to remove all pointers from the parser stack
           to structures that were built by semantic actions during parsing.
           This is optional, but lowers the memory requirements of the
           programs. *)
(*e: signature Parsing.clear_parser (yacc) *)

(*s: exception Parsing.Parse_error (yacc) *)
exception Parse_error
        (* Raised when a parser encounters a syntax error.
           Can also be raised from the action part of a grammar rule,
           to initiate error recovery. *)
(*e: exception Parsing.Parse_error (yacc) *)

(*--*)

(* The following definitions are used by the generated parsers only.
   They are not intended to be used by user programs. *)

type parser_env

(*s: enum Parsing.parse_tables (yacc) *)
type parse_tables =
  { actions : (parser_env -> Obj.t) array;
    transl_const : int array;
    transl_block : int array;
    lhs : string;
    len : string;
    defred : string;
    dgoto : string;
    sindex : string;
    rindex : string;
    gindex : string;
    tablesize : int;
    table : string;
    check : string;
    error_function : string -> unit }
(*e: enum Parsing.parse_tables (yacc) *)

(*s: exception Parsing.YYexit (yacc) *)
exception YYexit of Obj.t
(*e: exception Parsing.YYexit (yacc) *)

(*s: signature Parsing.yyparse (yacc) *)
val yyparse :
      parse_tables -> int -> (Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> 'b
(*e: signature Parsing.yyparse (yacc) *)
(*s: signature Parsing.peek_val (yacc) *)
val peek_val : parser_env -> int -> 'a
(*e: signature Parsing.peek_val (yacc) *)
(*s: signature Parsing.is_current_lookahead (yacc) *)
val is_current_lookahead : 'a -> bool
(*e: signature Parsing.is_current_lookahead (yacc) *)
(*s: signature Parsing.parse_error (yacc) *)
val parse_error : string -> unit
(*e: signature Parsing.parse_error (yacc) *)



(*s: enum Parsing.stateid (yacc) *)
(* functions and types used by the generated parsers using the simple code
 * generation method *)

type stateid = S of int
(*e: enum Parsing.stateid (yacc) *)
(*s: enum Parsing.nonterm (yacc) *)
type nonterm = NT of string
(* index in the rule actions table passed to yyparse *)
(*e: enum Parsing.nonterm (yacc) *)
(*s: enum Parsing.rule_action (yacc) *)
(* index in the rule actions table passed to yyparse *)
type rule_action = RA of int
(*e: enum Parsing.rule_action (yacc) *)
(*s: enum Parsing.action (yacc) *)
type action = 
  | Shift of stateid
  | Reduce of nonterm * int (* size of rhs of the rule *) * rule_action
  | Accept
(*e: enum Parsing.action (yacc) *)

(*s: enum Parsing.lr_tables (yacc) *)
type 'tok lr_tables = {
  action: stateid * 'tok -> action;
  goto: stateid * nonterm -> stateid;
}
(*e: enum Parsing.lr_tables (yacc) *)

type parser_env_simple
(*s: enum Parsing.rules_actions (yacc) *)
type rules_actions = (parser_env_simple -> Obj.t) array
(*e: enum Parsing.rules_actions (yacc) *)

(*s: signature Parsing.peek_val_simple (yacc) *)
val peek_val_simple: parser_env_simple -> int -> 'a
(*e: signature Parsing.peek_val_simple (yacc) *)


(*s: signature Parsing.yyparse_simple (yacc) *)
val yyparse_simple:
  'tok lr_tables -> rules_actions ->
  (Lexing.lexbuf -> 'tok) -> ('tok -> string) -> Lexing.lexbuf -> 'a
(*e: signature Parsing.yyparse_simple (yacc) *)

(*e: stdlib/parsing.mli *)
