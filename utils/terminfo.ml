(*s: utils/terminfo.ml *)
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

(* Basic interface to the terminfo database *)

external setupterm: unit -> unit = "terminfo_setup"
external getstr: string -> string = "terminfo_getstr"
external getnum: string -> int = "terminfo_getnum"
external puts: out_channel -> string -> int -> unit = "terminfo_puts"

(*e: utils/terminfo.ml *)
