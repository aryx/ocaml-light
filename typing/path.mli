(*s: ./typing/path.mli *)
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

(* $Id: path.mli,v 1.5 1996/07/15 16:35:34 xleroy Exp $ *)

(*s: type Path.t *)
(* Access paths *)

type t =
    Pident of Ident.t
  | Pdot of t * string * int
(*e: type Path.t *)

(*s: signature Path.same *)
val same: t -> t -> bool
(*e: signature Path.same *)
(*s: signature Path.isfree *)
val isfree: Ident.t -> t -> bool
(*e: signature Path.isfree *)
(*s: signature Path.binding_time *)
val binding_time: t -> int
(*e: signature Path.binding_time *)

(*s: signature Path.nopos *)
val nopos: int
(*e: signature Path.nopos *)
(*e: ./typing/path.mli *)
