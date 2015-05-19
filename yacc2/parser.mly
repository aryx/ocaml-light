%{
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

%}

%token <Ast.term> TTerm
%token <Ast.nonterm> TNonterm
%token Ttoken Tprec Tstart Ttype
%token TColon TOr TSemicolon
%token <string> TAngle
%token <Ast.location> TAction
%token TEOF

%start parser_definition
%type <Ast.parser_definition> parser_definition

%%

parser_definition: header directives_opt grammar header_opt TEOF 
  { { header = $1; directives = $2; grm = $3; trailer = $4 } }
;

header: TAction { $1 }
;

directive:
   Ttoken type_opt terms { $3 |> List.map (fun t -> Token ($2, t)) }
 | Tprec                 { [Prec ()] }
 | Tstart TNonterm       { [Start $2] }
 | Ttype TAngle TNonterm { [Type ($2, $3)] }
;


grammar: rules_opt { $1 }
;

rules_opt: 
                   { [] }
 | rule_ rules_opt { $1 @ $2 }
;

rule_: TNonterm TColon cases TSemicolon 
  { $3 |> List.map (fun (case, action) -> 
    { lhs = $1; rhs = case; act = action }) 
  }
;

cases: 
   symbols_opt TAction           { [$1, $2] }
 | symbols_opt TAction TOr cases { ($1, $2)::$4 }
;


symbols_opt:
                      { [] }
 | symbol symbols_opt { $1::$2 }
;

symbol: 
   TTerm    { Term $1 }
 | TNonterm { Nonterm $1 }
;



terms: TTerm terms_opt { $1::$2 }
;

terms_opt:
                   { [] }
 | TTerm terms_opt { $1::$2 }
;

directives_opt:
                            { [] }
 | directive directives_opt { $1 @ $2 }
;



type_opt:
          { None }
 | TAngle { Some $1 }  
;

header_opt:
          { Location(0,0) }
 | header { $1 }  
;
