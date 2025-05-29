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

(* $Id: translclass.mli,v 1.3 1996/08/13 15:10:33 vouillon Exp $ *)

open Typedtree
open Lambda

val class_stub : lambda
val transl_class : Ident.t -> class_def -> lambda;;
