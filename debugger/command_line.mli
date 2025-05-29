(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: command_line.mli,v 1.1 1997/05/08 18:00:16 doligez Exp $ *)

(************************ Reading and executing commands ***************)

open Lexing;;

val interprete_line : string -> bool;;
val line_loop : lexbuf -> unit;;
