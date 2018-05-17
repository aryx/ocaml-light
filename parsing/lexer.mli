(*s: parsing/lexer.mli *)
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

(*s: signature [[Lexer.token]] *)
(* The lexical analyzer *)

val token: Lexing.lexbuf -> Parser.token
(*e: signature [[Lexer.token]] *)

(*s: type [[Lexer.error]] *)
type error =
    Illegal_character
  | Unterminated_comment
  | Unterminated_string
(*e: type [[Lexer.error]] *)

(*s: exception [[Lexer.Error]] *)
exception Error of error * int * int
(*e: exception [[Lexer.Error]] *)

(*s: signature [[Lexer.report_error]] *)
val report_error: error -> unit
(*e: signature [[Lexer.report_error]] *)

(*e: parsing/lexer.mli *)
