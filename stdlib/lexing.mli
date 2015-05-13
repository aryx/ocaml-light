(*s: stdlib/lexing.mli *)
(*s: copyright ocamllex *)
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
(*e: copyright ocamllex *)
(* Module [Lexing]: the run-time library for lexers generated by [camllex] *)

(*** Lexer buffers *)

(*s: type Lexing.lexbuf *)
(* The run-time library for lexers generated by camllex *)
(* The type of lexer buffers. A lexer buffer is the argument passed
   to the scanning functions defined by the generated scanners.
   The lexer buffer holds the current state of the scanner, plus
   a function to refill the buffer from the input. *)
type lexbuf =
  { refill_buff : lexbuf -> unit;

    mutable lex_buffer : string;
    mutable lex_buffer_len : int;

    mutable lex_abs_pos : int;
    mutable lex_start_pos : int;
    mutable lex_curr_pos : int;
    mutable lex_last_pos : int;

    mutable lex_last_action : int;
    mutable lex_eof_reached : bool 
  }
(*e: type Lexing.lexbuf *)

(*s: signature Lexing.from_channel *)
val from_channel : in_channel -> lexbuf
        (* Create a lexer buffer on the given input channel.
           [Lexing.from_channel inchan] returns a lexer buffer which reads
           from the input channel [inchan], at the current reading position. *)
(*e: signature Lexing.from_channel *)
(*s: signature Lexing.from_string *)
val from_string : string -> lexbuf
        (* Create a lexer buffer which reads from
           the given string. Reading starts from the first character in
           the string. An end-of-input condition is generated when the
           end of the string is reached. *)
(*e: signature Lexing.from_string *)
(*s: signature Lexing.from_function *)
val from_function : (string -> int -> int) -> lexbuf
        (* Create a lexer buffer with the given function as its reading method.
           When the scanner needs more characters, it will call the given
           function, giving it a character string [s] and a character
           count [n]. The function should put [n] characters or less in [s],
           starting at character number 0, and return the number of characters
           provided. A return value of 0 means end of input. *)
(*e: signature Lexing.from_function *)

(*** Functions for lexer semantic actions *)

        (* The following functions can be called from the semantic actions
           of lexer definitions (the ML code enclosed in braces that
           computes the value returned by lexing functions). They give
           access to the character string matched by the regular expression
           associated with the semantic action. These functions must be
           applied to the argument [lexbuf], which, in the code generated by
           [camllex], is bound to the lexer buffer passed to the parsing
           function. *)

(*s: signature Lexing.lexeme *)
val lexeme : lexbuf -> string
        (* [Lexing.lexeme lexbuf] returns the string matched by
           the regular expression. *)
(*e: signature Lexing.lexeme *)
(*s: signature Lexing.lexeme_char *)
val lexeme_char : lexbuf -> int -> char
        (* [Lexing.lexeme_char lexbuf i] returns character number [i] in
           the matched string. *)
(*e: signature Lexing.lexeme_char *)
(*s: signature Lexing.lexeme_start *)
val lexeme_start : lexbuf -> int
        (* [Lexing.lexeme_start lexbuf] returns the position in the
           input stream of the first character of the matched string.
           The first character of the stream has position 0. *)
(*e: signature Lexing.lexeme_start *)
(*s: signature Lexing.lexeme_end *)
val lexeme_end : lexbuf -> int
        (* [Lexing.lexeme_end lexbuf] returns the position in the input stream
           of the character following the last character of the matched
           string. The first character of the stream has position 0. *)
(*e: signature Lexing.lexeme_end *)

(*--*)

(*s: type Lexing.lex_tables *)
(* The following definitions are used by the generated scanners only.
   They are not intended to be used by user programs. *)

type lex_tables =
  { lex_base: string;
    lex_backtrk: string;
    lex_default: string;
    lex_trans: string;
    lex_check: string }
(*e: type Lexing.lex_tables *)

(* take an integer representing a state and return an integer representing
 * an action_id
 *)
external engine: lex_tables -> int -> lexbuf -> int = "lex_engine"
(*e: stdlib/lexing.mli *)
