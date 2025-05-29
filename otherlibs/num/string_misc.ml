(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*    Valerie Menissier-Morain, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: string_misc.ml,v 1.2 1996/04/30 14:47:11 xleroy Exp $ *)

let rec index_char str chr pos =
  if pos >= String.length str then -1
  else if String.get str pos = chr then pos
  else index_char str chr (pos + 1)
;;
