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

(* $Id: show_source.mli,v 1.1 1996/11/29 16:55:07 xleroy Exp $ *)

(* Print the line containing the point *)
val show_point : string -> int -> bool -> bool -> unit;;

(* Tell Emacs we are nowhere in the source. *)
val show_no_point : unit -> unit;;

(* Display part of the source. *)
val show_listing : string -> int -> int -> int -> bool -> unit;;
