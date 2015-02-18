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

(* $Id: printexc.mli,v 1.6 1997/06/12 15:29:01 doligez Exp $ *)

(* Module [Printexc]: a catch-all exception handler *)

val catch: ('a -> 'b) -> 'a -> 'b
        (* [Printexc.catch fn x] applies [fn] to [x] and returns the result.
           If the evaluation of [fn x] raises any exception, the
           name of the exception is printed on standard error output,
           and the programs aborts with exit code 2.
           Typical use is [Printexc.catch main ()], where [main], with type
           [unit->unit], is the entry point of a standalone program.
           This catches and reports any exception that escapes the program. *)

val print: ('a -> 'b) -> 'a -> 'b
        (* Same as [catch], but re-raise the stray exception after
           printing it, instead of aborting the program. *)

val to_string : exn -> string
        (* [Printexc.to_string e] returns a string representation of [e]. *)
