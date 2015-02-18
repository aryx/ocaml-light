(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: condition.ml,v 1.4 1997/09/01 14:26:15 xleroy Exp $ *)

type t
external create: unit -> t = "caml_condition_new"
external wait: t -> Mutex.t -> unit = "caml_condition_wait"
external signal: t -> unit = "caml_condition_signal"
external broadcast: t -> unit = "caml_condition_broadcast"
