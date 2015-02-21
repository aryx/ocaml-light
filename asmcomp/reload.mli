(*s: asmcomp/reload.mli *)
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

(* $Id: reload.mli,v 1.5 1997/07/24 11:48:54 xleroy Exp $ *)

(*s: signature Reload.fundecl *)
(* Insert load/stores for pseudoregs that got assigned to stack locations. *)

val fundecl: Mach.fundecl -> Mach.fundecl * bool
(*e: signature Reload.fundecl *)

(*e: asmcomp/reload.mli *)
