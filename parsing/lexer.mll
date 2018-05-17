(*s: ./parsing/lexer.mll *)
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

(* The lexer definition *)

{
open Misc
open Parser

(*s: type [[Lexer.error]] *)
type error =
    Illegal_character
  | Unterminated_comment
  | Unterminated_string
(*e: type [[Lexer.error]] *)
(*s: exception [[Lexer.Error]] *)
exception Error of error * int * int
(*e: exception [[Lexer.Error]] *)

(*s: Lexer helpers *)
(* For nested comments *)

(*s: global [[Lexer.comment_depth]] *)
let comment_depth = ref 0
(*e: global [[Lexer.comment_depth]] *)

(* The table of keywords *)

(*s: constant [[Lexer.keyword_table]] *)
let keyword_table =
  create_hashtable 149 [
    "true", TRUE;
    "false", FALSE;

    "let", LET;
    "rec", REC;
    "in", IN;
    "fun", FUN;
    "function", FUNCTION;

    "if", IF;
    "then", THEN;
    "else", ELSE;
    "begin", BEGIN;
    "end", END;
    "while", WHILE;
    "for", FOR;
    "do", DO;
    "done", DONE;
    "to", TO;
    "downto", DOWNTO;

    "match", MATCH;
    "with", WITH;
    "when", WHEN;
    "as", AS;

    "try", TRY;
    "exception", EXCEPTION;

    "type", TYPE;
    "of", OF;
    "val", VAL;
    "external", EXTERNAL;
    "mutable", MUTABLE;

    "module", MODULE;
    "sig", SIG;
    "struct", STRUCT;
    "open", OPEN;

    "mod",  INFIXOP3("mod");
    "land", INFIXOP3("land");
    "lor",  INFIXOP3("lor");
    "lxor", INFIXOP3("lxor");
    "lsl",  INFIXOP4("lsl");
    "lsr",  INFIXOP4("lsr");
    "asr",  INFIXOP4("asr");

    (*s: [[Lexer.keyword_table]] elements *)
    "and", AND;
    "or", OR;
    (*x: [[Lexer.keyword_table]] elements *)
    "assert", ASSERT;
    (*x: [[Lexer.keyword_table]] elements *)
    "lazy", LAZY;
    (*e: [[Lexer.keyword_table]] elements *)
]
(*e: constant [[Lexer.keyword_table]] *)

(* To buffer string literals *)

(*s: Lexer string related functions *)
let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s
(*e: Lexer string related functions *)

(* To translate escape sequences *)

(*s: Lexer escape sequences related functions *)
let char_for_backslash =
  match Sys.os_type with
  | "Unix" ->
      begin function
      | 'n' -> '\010'
      | 'r' -> '\013'
      | 'b' -> '\008'
      | 't' -> '\009'
      | c   -> c
      end
  | x -> fatal_error "Lexer: unknown system type"
(*x: Lexer escape sequences related functions *)
let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf (i+0)) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) 
  in  
  Char.chr(c land 0xFF)
(*e: Lexer escape sequences related functions *)

(* To store the position of the beginning of a string or comment *)

(*s: global [[Lexer.start_pos]] *)
let start_pos = ref 0
(*e: global [[Lexer.start_pos]] *)
(*e: Lexer helpers *)

(* Error report *)
open Format
(*s: function [[Lexer.report_error]] *)
let report_error = function
    Illegal_character ->
      print_string "Illegal character"
  | Unterminated_comment ->
      print_string "Comment not terminated"
  | Unterminated_string ->
      print_string "String literal not terminated"
(*e: function [[Lexer.report_error]] *)
}

(*s: rule Lexer.token *)
rule token = parse
  (*s: [[Lexer.token()]] space case *)
    [' ' '\010' '\013' '\009' '\012'] +
      { token lexbuf }
  (*e: [[Lexer.token()]] space case *)
  (*s: [[Lexer.token()]] comment case *)
  | "(*"
      { comment_depth := 1;
        start_pos := Lexing.lexeme_start lexbuf;
        comment lexbuf;
        token lexbuf }
  (*e: [[Lexer.token()]] comment case *)

  (*s: [[Lexer.token()]] underscore case *)
  | "_"  { UNDERSCORE }
  (*e: [[Lexer.token()]] underscore case *)
  (*s: [[Lexer.token()]] identifier or keyword cases *)
  | ['a'-'z'  '_'] (['A'-'Z' 'a'-'z' '_' '\'' '0'-'9' ]) *
      { let s = Lexing.lexeme lexbuf in
          try
            Hashtbl.find keyword_table s
          with Not_found ->
            LIDENT s 
       }
  | ['A'-'Z'  ] (['A'-'Z' 'a'-'z' '_' '\'' '0'-'9' ]) *
      { UIDENT(Lexing.lexeme lexbuf) }       (* No capitalized keywords *)
  (*e: [[Lexer.token()]] identifier or keyword cases *)

  (*s: [[Lexer.token()]] integer case *)
  | ['0'-'9']+
  | '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
  | '0' ['o' 'O'] ['0'-'7']+
  | '0' ['b' 'B'] ['0'-'1']+
      { INT (int_of_string(Lexing.lexeme lexbuf)) }
  (*e: [[Lexer.token()]] integer case *)
  (*s: [[Lexer.token()]] float case *)
  | ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?
      { FLOAT (Lexing.lexeme lexbuf) }
  (*e: [[Lexer.token()]] float case *)

  (*s: [[Lexer.token()]] string case *)
  | "\""
      { reset_string_buffer();
        let string_start = Lexing.lexeme_start lexbuf in
        start_pos := string_start;

        string lexbuf;

        lexbuf.Lexing.lex_start_pos <-
          string_start - lexbuf.Lexing.lex_abs_pos;
        STRING (get_stored_string()) }
  (*e: [[Lexer.token()]] string case *)
  (*s: [[Lexer.token()]] character case *)
  | "'" [^ '\\' '\''] "'"
      { CHAR(Lexing.lexeme_char lexbuf 1) }
  (*x: [[Lexer.token()]] character case *)
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { CHAR(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  (*x: [[Lexer.token()]] character case *)
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { CHAR(char_for_decimal_code lexbuf 2) }
  (*e: [[Lexer.token()]] character case *)

  (*s: [[Lexer.token()]] directive case *)
  | "#" [' ' '\t']* ['0'-'9']+ [' ' '\t']* "\"" [^ '\n' '\r'] *
    ('\n' | '\r' | "\r\n")
      (* # linenum "filename" flags \n *)
      { token lexbuf }
  (*e: [[Lexer.token()]] directive case *)
  (*s: [[Lexer.token()]] sharp case *)
  | "#"  { SHARP }
  (*e: [[Lexer.token()]] sharp case *)

  (*s: [[Lexer.token()]] operator cases *)
  | "("  { LPAREN } | ")"  { RPAREN }
  | "{"  { LBRACE } | "}"  { RBRACE }
  | "["  { LBRACKET } | "]"  { RBRACKET }
  (*x: [[Lexer.token()]] operator cases *)
  | "|"  { BAR }
  | "*"  { STAR }
  | "'"  { QUOTE }
  (*x: [[Lexer.token()]] operator cases *)
  | ","  { COMMA }
  | "->" { MINUSGREATER }
  | "."  { DOT }
  | ":"  { COLON }
  | "::" { COLONCOLON }
  | ":=" { COLONEQUAL }
  | "<-" { LESSMINUS }
  | ";"  { SEMI }
  (*x: [[Lexer.token()]] operator cases *)
  | "="  { EQUAL }
  | "!=" { INFIXOP0 "!=" }
  | "<"  { LESS } | ">"  { GREATER }
  (*x: [[Lexer.token()]] operator cases *)
  | "&&" { AMPERAMPER }
  | "||" { BARBAR }
  (*x: [[Lexer.token()]] operator cases *)
  | "&"  { AMPERSAND }
  (*x: [[Lexer.token()]] operator cases *)
  | "-"  { SUBTRACTIVE "-" }
  | "-." { SUBTRACTIVE "-." }
  (*x: [[Lexer.token()]] operator cases *)
  | ['!' '?' '~']
    ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'] *
            { PREFIXOP(Lexing.lexeme lexbuf) }

  | ['=' '<' '>' '|' '&' '$']
    ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'] *
            { INFIXOP0(Lexing.lexeme lexbuf) }
  | ['@' '^']
    ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'] *
            { INFIXOP1(Lexing.lexeme lexbuf) }
  | ['+' '-']
    ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'] *
            { INFIXOP2(Lexing.lexeme lexbuf) }
  | "**"
    ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'] *
            { INFIXOP4(Lexing.lexeme lexbuf) }
  | ['*' '/' '%']
    ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'] *
            { INFIXOP3(Lexing.lexeme lexbuf) }
  (*x: [[Lexer.token()]] operator cases *)
  | "[|" { LBRACKETBAR } | "|]" { BARRBRACKET }
  (*x: [[Lexer.token()]] operator cases *)
  | ".." { DOTDOT }
  (*x: [[Lexer.token()]] operator cases *)
  | ";;" { SEMISEMI }
  (*e: [[Lexer.token()]] operator cases *)
  | eof { EOF }
  | _
      { raise (Error(Illegal_character,
                     Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) }

(*e: rule Lexer.token *)

(*s: rule Lexer.comment *)
and comment = parse
    "(*"
      { comment_depth := succ !comment_depth; comment lexbuf }
  | "*)"
      { comment_depth := pred !comment_depth;
        if !comment_depth > 0 then comment lexbuf }
  (*s: [[Lexer.comment()]] string or char in comment cases *)
  | "\""
      { reset_string_buffer();
        start_pos := Lexing.lexeme_start lexbuf;
        string lexbuf;
        string_buff := initial_string_buffer;
        comment lexbuf }
  | "''"
      { comment lexbuf }
  | "'" [^ '\\' '\''] "'"
      { comment lexbuf }
  | "'\\" ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { comment lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { comment lexbuf }
  (*e: [[Lexer.comment()]] string or char in comment cases *)
  | eof
      { raise (Error(Unterminated_comment, !start_pos, !start_pos+2)) }
  | _
      { comment lexbuf }
(*e: rule Lexer.comment *)

(*s: rule Lexer.string *)
and string = parse
    '"'
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
        string lexbuf }
  | eof
      { raise (Error(Unterminated_string, !start_pos, !start_pos+1)) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }
(*e: rule Lexer.string *)

(*e: ./parsing/lexer.mll *)
