(*s: ./parsing/location.mli *)
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

(*s: type [[Location.t]] *)
(* Source code locations, used in parsetree *)

type t =
  { loc_start: int; loc_end: int }
(*e: type [[Location.t]] *)

(*s: signature [[Location.none]] *)
val none: t
(*e: signature [[Location.none]] *)
(*s: signature [[Location.symbol_loc]] *)
val symbol_loc: unit -> t
(*e: signature [[Location.symbol_loc]] *)
(*s: signature [[Location.rhs_loc]] *)
val rhs_loc: int -> t
(*e: signature [[Location.rhs_loc]] *)

(*s: signature [[Location.input_name]] *)
val input_name: string ref
(*e: signature [[Location.input_name]] *)
(*s: signature [[Location.input_lexbuf]] *)
val input_lexbuf: Lexing.lexbuf option ref
(*e: signature [[Location.input_lexbuf]] *)

(*s: signature [[Location.print]] *)
val print: t -> unit
(*e: signature [[Location.print]] *)
(*s: signature [[Location.print_warning]] *)
val print_warning: t -> string -> unit
(*e: signature [[Location.print_warning]] *)
(*s: signature [[Location.echo_eof]] *)
val echo_eof: unit -> unit
(*e: signature [[Location.echo_eof]] *)
(*s: signature [[Location.reset]] *)
val reset: unit -> unit
(*e: signature [[Location.reset]] *)

(*s: signature [[Location.highlight_locations]] *)
val highlight_locations: t -> t -> bool
(*e: signature [[Location.highlight_locations]] *)
(*e: ./parsing/location.mli *)
