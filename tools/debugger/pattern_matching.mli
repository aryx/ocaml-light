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

(* $Id: pattern_matching.mli,v 1.1 1996/11/29 16:55:05 xleroy Exp $ *)

(************************ Simple pattern matching **********************)

open Parser_aux

val pattern_matching :
  pattern -> Debugcom.remote_value -> Typedtree.type_expr -> (string * Debugcom.remote_value * Typedtree.type_expr) list;;
