(*s: ./typing/path.mli *)
(*s: copyright header0 *)
(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header0 *)

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

(*s: signature Path.nopos *)
val nopos: int
(*e: signature Path.nopos *)
(*e: ./typing/path.mli *)
