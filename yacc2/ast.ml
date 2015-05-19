(* Yoann Padioleau
 *
 * Copyright (C) 2015 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* uppercase string *)
type term = T of string
(* lowercase string *)
type nonterm = NT of string

type symbol = Term of term | Nonterm of nonterm

type charpos = int
type location =
    Location of charpos * charpos
(* todo: at some point need to parse to extract the $xxx *)
type action = location

type grammar = rule_ list
  and rule_ = {
    lhs: nonterm;
    rhs: symbol list;
    act: action;
  }

type directive =
  | Token of type_ option * term
  | Start of nonterm
  | Type of type_ * nonterm

  | Prec of unit (* TODO *)

  and type_ = string

(* main data structure *)
type parser_definition = {
  header: location;
  directives: directive list;
  grm: grammar;
  trailer: location;
}

(* for the augmented grammar *)

(* They should not conflict with user-defined terminals or non terminals
 * because nonterminals cannot contain '$' according to lexer.mll and 
 * terminals must start with an uppercase letter according again
 * to lexer.mll
 *)
let start_nonterminal = NT "S$"
let dollar_terminal = T "$"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let start_symbol def =
  try 
    (match
      def.directives |> List.find (function
        | Start x -> true
        | _ -> false
      )
     with
     | Start x -> x
     | _ -> failwith "impossible"
    )
  with Not_found -> failwith "no start symbol found"
