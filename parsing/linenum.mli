(*s: parsing/linenum.mli *)
(*s: copyright header 1997 *)
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
(*e: copyright header 1997 *)

(*s: signature [[Linenum.for_position]] *)
(* An auxiliary lexer for determining the line number corresponding to
   a file position, honoring the directives # linenum "filename" *)

val for_position: string -> int -> string * int * int
        (* [Linenum.for_position file loc] returns a triple describing
           the location [loc] in the file named [file].
           First result is name of actual source file.
           Second result is line number in that source file.
           Third result is position of beginning of that line in [file]. *)
(*e: signature [[Linenum.for_position]] *)
(*e: parsing/linenum.mli *)
