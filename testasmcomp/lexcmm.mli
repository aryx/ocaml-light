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

val token: Lexing.lexbuf -> Parsecmm.token

type error =
    Illegal_character
  | Unterminated_comment
  | Unterminated_string

exception Error of error

val report_error: Lexing.lexbuf -> error -> unit
