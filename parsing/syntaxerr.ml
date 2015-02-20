(*s: ./parsing/syntaxerr.ml *)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


(* Auxiliary type for reporting syntax errors *)

open Format

(*s: type Syntaxerr.error (./parsing/syntaxerr.ml) *)
type error =
    Unclosed of Location.t * string * Location.t * string
  | Other of Location.t
(*e: type Syntaxerr.error (./parsing/syntaxerr.ml) *)

(*s: exception Syntaxerr.Error (./parsing/syntaxerr.ml) *)
exception Error of error
(*e: exception Syntaxerr.Error (./parsing/syntaxerr.ml) *)
(*s: exception Syntaxerr.Escape_error (./parsing/syntaxerr.ml) *)
exception Escape_error
(*e: exception Syntaxerr.Escape_error (./parsing/syntaxerr.ml) *)

(*s: constant Syntaxerr.report_error *)
let report_error = function
    Unclosed(opening_loc, opening, closing_loc, closing) ->
      if String.length !Location.input_name = 0
      && Location.highlight_locations opening_loc closing_loc
      then begin
        print_string "Syntax error: '";
        print_string closing;
        print_string "' expected, the highlighted '";
        print_string opening;
        print_string "' might be unmatched"
      end else begin
        Location.print closing_loc;
        print_string "Syntax error: '";
        print_string closing;
        print_string "' expected"; force_newline();
        Location.print opening_loc;
        print_string "This '";
        print_string opening;
        print_string "' might be unmatched"
      end
  | Other loc ->
      Location.print loc;
      print_string "Syntax error"
(*e: constant Syntaxerr.report_error *)


(*e: ./parsing/syntaxerr.ml *)
