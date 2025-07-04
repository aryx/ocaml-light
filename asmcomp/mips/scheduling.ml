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

(* $Id: scheduling.ml,v 1.1 1997/07/24 11:49:09 xleroy Exp $ *)

open Schedgen (* to create a dependency *)

(* No scheduling is needed for the Mips, the assembler
   does it better than us.  *)

let fundecl f = f
