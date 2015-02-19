(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: genprintval.mli,v 1.1 1997/03/22 20:16:47 vouillon Exp $ *)

(* Printing of values *)

open Types

val install_printer : Path.t -> Types.type_expr -> (Obj.t -> unit) -> unit
val remove_printer : Path.t -> unit

val print_exception : Obj.t -> unit
val print_value :
      int -> int -> (int -> Obj.t -> Types.type_expr -> bool) ->
      Env.t -> Obj.t -> type_expr -> unit
