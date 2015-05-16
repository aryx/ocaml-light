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
open Ast

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 *
 * todo:
 *  - handle priorities
 *  - EBNF support!
 *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () =

  if Array.length Sys.argv != 2 then begin
    prerr_endline "Usage: ocamlyacc <input file>";
    exit 2
  end;

  let source_name = Sys.argv.(1) in

  let dest_name =
    if Filename.check_suffix source_name ".mly" 
    then Filename.chop_suffix source_name ".mly" ^ ".ml"
    else source_name ^ ".ml" 
  in
  let ic = open_in source_name in
(*  let oc = open_out dest_name in *)
  let lexbuf = Lexing.from_channel ic in

  (* parsing *)
  let def =
    try
      Parser.parser_definition Lexer.main lexbuf
    with exn ->
(*      close_out oc; *)
      Sys.remove dest_name;
       (match exn with
         Parsing.Parse_error ->
           prerr_string "Syntax error around char ";
           prerr_int (Lexing.lexeme_start lexbuf);
           prerr_endline "."
       | Lexer.Lexical_error s ->
           prerr_string "Lexical error around char ";
           prerr_int (Lexing.lexeme_start lexbuf);
           prerr_string ": ";
           prerr_string s;
           prerr_endline "."
       | _ -> raise exn
       );
      exit 2 
  in
  let augmented =
    ({lhs_ = NT "$s"; rhs = [Nonterm (Ast.start_symbol def)]; 
     act = Location(0,0)}
    :: def.grm) |> Array.of_list
  in
  let env = { Lr0.g = augmented } in
  let automaton = Lr0.canonical_lr0_automaton env in
  Dump.dump_lr0_automaton env automaton;
  let tables = Slr.lr_tables env automaton in

  ()

let _ = 
  Tests.test_lr0 ();
  Printexc.catch main (); 
  exit 0
