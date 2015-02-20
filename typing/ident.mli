(*s: ./typing/ident.mli *)
(*s: copyright header *)
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
(*e: copyright header *)

(* Identifiers (unique names) *)

type t

(*s: signature Ident.create *)
val create: string -> t
(*e: signature Ident.create *)
(*s: signature Ident.create_persistent *)
val create_persistent: string -> t
(*e: signature Ident.create_persistent *)
(*s: signature Ident.name *)
val name: t -> string
(*e: signature Ident.name *)
(*s: signature Ident.unique_name *)
val unique_name: t -> string
(*e: signature Ident.unique_name *)
(*s: signature Ident.persistent *)
val persistent: t -> bool
(*e: signature Ident.persistent *)
(*s: signature Ident.equal *)
val equal: t -> t -> bool
        (* Compare identifiers by name. *)      
(*e: signature Ident.equal *)
(*s: signature Ident.same *)
val same: t -> t -> bool
        (* Compare identifiers by binding location.
           Two identifiers are the same either if they are both
           non-persistent and have been created by the same call to
           [new], or if they are both persistent and have the same
           name. *)
(*e: signature Ident.same *)
(*s: signature Ident.hide *)
val hide: t -> t
        (* Return an identifier with same name as the given identifier,
           but stamp different from any stamp returns by new.
           When put in a 'a tbl, this identifier can only be looked
           up by name. *)
(*e: signature Ident.hide *)

(*s: signature Ident.make_global *)
val make_global: t -> unit
(*e: signature Ident.make_global *)
(*s: signature Ident.global *)
val global: t -> bool
(*e: signature Ident.global *)

(*s: signature Ident.binding_time *)
val binding_time: t -> int
(*e: signature Ident.binding_time *)
(*s: signature Ident.current_time *)
val current_time: unit -> int
(*e: signature Ident.current_time *)

(*s: signature Ident.print *)
val print: t -> unit
(*e: signature Ident.print *)

type 'a tbl
        (* Association tables from identifiers to type 'a. *)

(*s: signature Ident.empty *)
val empty: 'a tbl
(*e: signature Ident.empty *)
(*s: signature Ident.add *)
val add: t -> 'a -> 'a tbl -> 'a tbl
(*e: signature Ident.add *)
(*s: signature Ident.find_same *)
val find_same: t -> 'a tbl -> 'a
(*e: signature Ident.find_same *)
(*s: signature Ident.find_name *)
val find_name: string -> 'a tbl -> 'a
(*e: signature Ident.find_name *)

(*s: signature Ident.iter *)
val iter: (t -> 'a -> unit) -> 'a tbl -> unit
(*e: signature Ident.iter *)
(*s: signature Ident.print_tbl *)
val print_tbl: ('a -> unit) -> 'a tbl -> unit
(*e: signature Ident.print_tbl *)
(*e: ./typing/ident.mli *)
