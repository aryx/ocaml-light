(*s: ./typing/ident.mli *)
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

(* Identifiers (unique names) *)

type t

(*s: signature [[Ident.create]] *)
val create: string -> t
(*e: signature [[Ident.create]] *)
(*s: signature [[Ident.create_persistent]] *)
val create_persistent: string -> t
(*e: signature [[Ident.create_persistent]] *)
(*s: signature [[Ident.name]] *)
val name: t -> string
(*e: signature [[Ident.name]] *)
(*s: signature [[Ident.unique_name]] *)
val unique_name: t -> string
(*e: signature [[Ident.unique_name]] *)
(*s: signature [[Ident.persistent]] *)
val persistent: t -> bool
(*e: signature [[Ident.persistent]] *)
(*s: signature [[Ident.same]] *)
val same: t -> t -> bool
        (* Compare identifiers by binding location.
           Two identifiers are the same either if they are both
           non-persistent and have been created by the same call to
           [new], or if they are both persistent and have the same
           name. *)
(*e: signature [[Ident.same]] *)
(*s: signature [[Ident.identify]] *)
val identify: t -> t -> (unit -> 'a) -> 'a
        (* [identify id1 id2 f] temporarily makes [id1] and [id2] the same
           during the evaluation of [f ()]. *)
(*e: signature [[Ident.identify]] *)
(*s: signature [[Ident.hide]] *)
val hide: t -> t
        (* Return an identifier with same name as the given identifier,
           but stamp different from any stamp returns by new.
           When put in a 'a tbl, this identifier can only be looked
           up by name. *)
(*e: signature [[Ident.hide]] *)

(*s: signature [[Ident.make_global]] *)
val make_global: t -> unit
(*e: signature [[Ident.make_global]] *)
(*s: signature [[Ident.global]] *)
val global: t -> bool
(*e: signature [[Ident.global]] *)

(*s: signature [[Ident.print]] *)
val print: t -> unit
(*e: signature [[Ident.print]] *)

type 'a tbl
        (* Association tables from identifiers to type 'a. *)

(*s: signature [[Ident.empty]] *)
val empty: 'a tbl
(*e: signature [[Ident.empty]] *)
(*s: signature [[Ident.add]] *)
val add: t -> 'a -> 'a tbl -> 'a tbl
(*e: signature [[Ident.add]] *)
(*s: signature [[Ident.find_same]] *)
val find_same: t -> 'a tbl -> 'a
(*e: signature [[Ident.find_same]] *)
(*s: signature [[Ident.find_name]] *)
val find_name: string -> 'a tbl -> 'a
(*e: signature [[Ident.find_name]] *)

(*s: signature [[Ident.print_tbl]] *)
val print_tbl: ('a -> unit) -> 'a tbl -> unit
(*e: signature [[Ident.print_tbl]] *)

(*e: ./typing/ident.mli *)
