{
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

open Ast (* Location *)
open Parser

exception Lexical_error of string

let comment_depth = ref 0
let brace_depth = ref 0
}

rule main = parse
| [' ' '\010' '\013' '\009' '\012' ] + 
    { main lexbuf }
| "(*" 
    { comment_depth := 1;
      comment lexbuf;
      main lexbuf }

| ['A'-'Z' ] ['A'-'Z' 'a'-'z' '\'' '_' '0'-'9'] *
    { TTerm (T (Lexing.lexeme lexbuf)) }
| ['a'-'z' ] ['A'-'Z' 'a'-'z' '\'' '_' '0'-'9'] *
    { TNonterm (NT (Lexing.lexeme lexbuf)) }
| '%' ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '\'' '_' '0'-'9'] *
    { match Lexing.lexeme lexbuf with
      | "%token" -> Ttoken
      | "%prec" -> Tprec
      | "%start" -> Tstart
      | "%type" -> Ttype
      | s -> failwith ("Unknown directive: " ^ s)
    }
(* to be backward compatible with ocamlyacc *)
| "%%" { main lexbuf }
| "%{" 
    { let n1 = Lexing.lexeme_end lexbuf in
      brace_depth := 1;
      let n2 = action2 lexbuf in
      TAction(Location(n1, n2)) }


| '{' 
    { let n1 = Lexing.lexeme_end lexbuf in
      brace_depth := 1;
      let n2 = action lexbuf in
      TAction(Location(n1, n2)) }

| '|'  { TOr }
| ':'  { TColon }
| ';'  { TSemicolon }

| '<'  { TAngle (angle lexbuf) }

| eof  { TEOF }
| _
    { raise(Lexical_error
             ("illegal character " ^ String.escaped(Lexing.lexeme lexbuf))) }

(* TODO: handle $x *)
and action = parse
| '{' 
    { incr brace_depth;
      action lexbuf }
| '}' 
    { decr brace_depth;
      if !brace_depth == 0 
      then Lexing.lexeme_start lexbuf 
      else action lexbuf }
| "(*" 
    { comment_depth := 1;
      comment lexbuf;
      action lexbuf }

| eof { raise (Lexical_error "unterminated action") }
| _   { action lexbuf }

and comment = parse
| "(*" 
    { incr comment_depth; 
      comment lexbuf }
| "*)" 
    { decr comment_depth;
      if !comment_depth == 0 
      then () 
      else comment lexbuf }

| eof { raise(Lexical_error "unterminated comment") }
| _ { comment lexbuf }

and angle = parse
| '>' { "" }
| eof { raise(Lexical_error "unterminated type") }
| [^'>']+ { let s = Lexing.lexeme lexbuf in s ^ angle lexbuf }
| _ { let s = Lexing.lexeme lexbuf in s ^ angle lexbuf }

(* to be backward compatible with ocamlyacc *)
and action2 = parse
| '{' 
    { incr brace_depth;
      action2 lexbuf }
| "%}" 
    { decr brace_depth;
      if !brace_depth == 0 
      then Lexing.lexeme_start lexbuf 
      else action2 lexbuf }
| "}" 
    { decr brace_depth;
      if !brace_depth == 0 
      then Lexing.lexeme_start lexbuf 
      else action2 lexbuf }
| "(*" 
    { comment_depth := 1;
      comment lexbuf;
      action2 lexbuf }

| eof { raise (Lexical_error "unterminated action") }
| _   { action2 lexbuf }
