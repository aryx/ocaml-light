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

(* $Id: ident.mli,v 1.8 1997/03/10 00:23:53 vouillon Exp $ *)

(* Identifiers (unique names) *)

type t

val create: string -> t
val create_persistent: string -> t
val name: t -> string
val unique_name: t -> string
val persistent: t -> bool
val equal: t -> t -> bool
        (* Compare identifiers by name. *)      
val same: t -> t -> bool
        (* Compare identifiers by binding location.
           Two identifiers are the same either if they are both
           non-persistent and have been created by the same call to
           [new], or if they are both persistent and have the same
           name. *)
val hide: t -> t
        (* Return an identifier with same name as the given identifier,
           but stamp different from any stamp returns by new.
           When put in a 'a tbl, this identifier can only be looked
           up by name. *)

val make_global: t -> unit
val global: t -> bool

val binding_time: t -> int
val current_time: unit -> int

val print: t -> unit

type 'a tbl
        (* Association tables from identifiers to type 'a. *)

val empty: 'a tbl
val add: t -> 'a -> 'a tbl -> 'a tbl
val find_same: t -> 'a tbl -> 'a
val find_name: string -> 'a tbl -> 'a

val iter: (t -> 'a -> unit) -> 'a tbl -> unit
val print_tbl: ('a -> unit) -> 'a tbl -> unit
