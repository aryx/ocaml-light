(*s: ./parsing/parse.ml *)
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

(* Entry points in the parser *)

open Location

(*s: function Parse.skip_phrase *)
(* Skip tokens to the end of the phrase *)
let rec skip_phrase lexbuf =
  try
    match Lexer.token lexbuf with
      Parser.SEMISEMI | Parser.EOF -> ()
    | _ -> skip_phrase lexbuf
  with
    | Lexer.Error (Lexer.Unterminated_comment, _, _) -> ()
    | Lexer.Error (Lexer.Unterminated_string, _, _) -> ()
    | Lexer.Error(_,_,_) -> skip_phrase lexbuf
(*e: function Parse.skip_phrase *)

(*s: function Parse.maybe_skip_phrase *)
let maybe_skip_phrase lexbuf =
  if Parsing.is_current_lookahead Parser.SEMISEMI
  or Parsing.is_current_lookahead Parser.EOF
  then ()
  else skip_phrase lexbuf
(*e: function Parse.maybe_skip_phrase *)

(*s: function Parse.wrap *)
let wrap parsing_fun lexbuf =
  try
    let ast = parsing_fun Lexer.token lexbuf in
    Parsing.clear_parser();
    ast
  with
    | Lexer.Error(Lexer.Unterminated_comment, _, _) as err -> raise err
    | Lexer.Error(Lexer.Unterminated_string, _, _) as err -> raise err
    | Lexer.Error(_, _, _) as err ->
        if !Location.input_name = "" then skip_phrase lexbuf;
        raise err
    | Syntaxerr.Error _ as err ->
        if !Location.input_name = "" then maybe_skip_phrase lexbuf;
        raise err
    | Parsing.Parse_error | Syntaxerr.Escape_error ->
        let loc = { loc_start = Lexing.lexeme_start lexbuf;
                    loc_end = Lexing.lexeme_end lexbuf } in
        if !Location.input_name = "" 
        then maybe_skip_phrase lexbuf;
        raise(Syntaxerr.Error(Syntaxerr.Other loc))
(*e: function Parse.wrap *)

(*s: function Parse.implementation *)
let implementation = wrap Parser.implementation
(*e: function Parse.implementation *)
(*s: function Parse.interface *)
let interface = wrap Parser.interface
(*e: function Parse.interface *)

(*s: function Parse.xxx *)
let toplevel_phrase = wrap Parser.toplevel_phrase
let use_file = wrap Parser.use_file
(*e: function Parse.xxx *)
(*e: ./parsing/parse.ml *)
