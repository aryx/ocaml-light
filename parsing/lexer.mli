(*s: ./parsing/lexer.mli *)
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

(* $Id: lexer.mli,v 1.4 1997/01/01 15:36:17 xleroy Exp $ *)

(*s: signature Lexer.token *)
(* The lexical analyzer *)

val token: Lexing.lexbuf -> Parser.token
(*e: signature Lexer.token *)

(*s: type Lexer.error *)
type error =
    Illegal_character
  | Unterminated_comment
  | Unterminated_string
(*e: type Lexer.error *)

(*s: exception Lexer.Error *)
exception Error of error * int * int
(*e: exception Lexer.Error *)

(*s: signature Lexer.report_error *)
val report_error: error -> unit
(*e: signature Lexer.report_error *)

(*e: ./parsing/lexer.mli *)
