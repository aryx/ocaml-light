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

(* $Id: terminfo.mli,v 1.4 1996/04/30 14:53:40 xleroy Exp $ *)

(* Basic interface to the terminfo database *)

external setupterm: unit -> unit = "terminfo_setup"
external getstr: string -> string = "terminfo_getstr"
external getnum: string -> int = "terminfo_getnum"
external puts: out_channel -> string -> int -> unit = "terminfo_puts"

