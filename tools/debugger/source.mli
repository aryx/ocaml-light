(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


(************************ Source management ****************************)

(*** Conversion function. ***)

val source_of_module: string -> string

(*** buffer cache ***)

type buffer

val buffer_max_count : int ref

val flush_buffer_list : unit -> unit

val get_buffer : string -> buffer

val buffer_content : buffer -> string
val buffer_length : buffer -> int

(*** Position conversions. ***)

(* Pair (position, line) where `position' is the position in character of *)
(* the beginning of the line (first character is 0) and `line' is its *)
(* number (first line number is 1). *)
type position = int * int

(* Position of the next linefeed after `pos'. *)
(* Position just after the buffer end if no linefeed found. *)
(* Raise `Out_of_range' if already there. *)
val next_linefeed : buffer -> int -> int

(* Go to next line. *)
val next_line : buffer -> position -> position

(* Convert a position in the buffer to a line number. *)
val line_of_pos : buffer -> int -> position

(* Convert a line number to a position. *)
val pos_of_line : buffer -> int -> position

(* Convert a coordinate (line / column) into a position. *)
(* --- The first line and column are line 1 and column 1. *)
val point_of_coord : buffer -> int -> int -> int
