(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*          Xavier Leroy and Pascal Cuoq, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


type t
external create: unit -> t = "caml_mutex_new"
external lock: t -> unit = "caml_mutex_lock"
external try_lock: t -> bool = "caml_mutex_try_lock"
external unlock: t -> unit = "caml_mutex_unlock"
