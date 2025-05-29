(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: translobj.mli,v 1.3 1996/05/16 16:10:09 vouillon Exp $ *)

val oo_prim: string -> Lambda.lambda

val meth: string -> Ident.t

val reset_labels: unit -> unit
val transl_label_init: Lambda.lambda -> Lambda.lambda
