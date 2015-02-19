(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*      Damien Doligez and Francois Rouaix, INRIA Rocquencourt         *)
(*   Ported to Objective Caml by John Malecki and Xavier Leroy         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: profiling.mli,v 1.3 1997/06/20 12:50:13 doligez Exp $ *)

(* Run-time library for profiled programs *)

val counters: (string * (string * int array)) list ref
