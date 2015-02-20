(*s: ./parsing/parse.mli *)
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

(* $Id: parse.mli,v 1.5 1997/08/22 08:55:39 xleroy Exp $ *)

(*s: signature Parse.implementation *)
(* Entry points in the parser *)

val implementation : Lexing.lexbuf -> Parsetree.structure
(*e: signature Parse.implementation *)
(*s: signature Parse.interface *)
val interface : Lexing.lexbuf -> Parsetree.signature
(*e: signature Parse.interface *)
(*s: signature Parse.toplevel_phrase *)
val toplevel_phrase : Lexing.lexbuf -> Parsetree.toplevel_phrase
(*e: signature Parse.toplevel_phrase *)
(*s: signature Parse.use_file *)
val use_file : Lexing.lexbuf -> Parsetree.toplevel_phrase list
(*e: signature Parse.use_file *)

(*e: ./parsing/parse.mli *)
