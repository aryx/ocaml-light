(*s: ./utils/misc.mli *)
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

(* $Id: misc.mli,v 1.11 1996/10/31 16:03:49 xleroy Exp $ *)

(*s: signature Misc.fatal_error *)
(* Miscellaneous useful types and functions *)

val fatal_error: string -> 'a
(*e: signature Misc.fatal_error *)
(*s: exception Misc.Fatal_error *)
exception Fatal_error
(*e: exception Misc.Fatal_error *)

(*s: signature Misc.map_end *)
val map_end: ('a -> 'b) -> 'a list -> 'b list -> 'b list
        (* [map_end f l t] is [map f l @ t], just more efficient. *)
(*e: signature Misc.map_end *)
(*s: signature Misc.for_all2 *)
val for_all2: ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
        (* Same as [List.for_all] but for a binary predicate. *)
(*e: signature Misc.for_all2 *)
(*s: signature Misc.filter *)
val filter: ('a -> bool) -> 'a list -> 'a list
(*e: signature Misc.filter *)
(*s: signature Misc.mem_assq *)
val mem_assq: 'a -> ('a * 'b) list -> bool
(*e: signature Misc.mem_assq *)
(*s: signature Misc.replicate_list *)
val replicate_list: 'a -> int -> 'a list
        (* [replicate_list elem n] is the list with [n] elements
           all identical to [elem]. *)
(*e: signature Misc.replicate_list *)

(*s: signature Misc.find_in_path *)
val find_in_path: string list -> string -> string
        (* Search a file in a list of directories. *)
(*e: signature Misc.find_in_path *)
(*s: signature Misc.remove_file *)
val remove_file: string -> unit
        (* Delete the given file if it exists. Never raise an error. *)
(*e: signature Misc.remove_file *)

(*s: signature Misc.create_hashtable *)
val create_hashtable: int -> ('a * 'b) list -> ('a, 'b) Hashtbl.t
        (* Create a hashtable of the given size and fills it with the
           given bindings. *)
(*e: signature Misc.create_hashtable *)

(*s: signature Misc.copy_file *)
val copy_file: in_channel -> out_channel -> unit
        (* [copy_file ic oc] reads the contents of file [ic] and copies
           them to [oc]. It stops when encountering EOF on [ic]. *)
(*e: signature Misc.copy_file *)
(*s: signature Misc.copy_file_chunk *)
val copy_file_chunk: in_channel -> out_channel -> int -> unit
        (* [copy_file_chunk ic oc n] reads [n] bytes from [ic] and copies
           them to [oc]. It raises [End_of_file] when encountering
           EOF on [ic]. *)
(*e: signature Misc.copy_file_chunk *)

(*s: signature Misc.log2 *)
val log2: int -> int
        (* [log2 n] returns [s] such that [n = 1 lsl s] 
           if [n] is a power of 2*)
(*e: signature Misc.log2 *)
(*s: signature Misc.align *)
val align: int -> int -> int
        (* [align n a] rounds [n] upwards to a multiple of [a]
           (a power of 2). *)
(*e: signature Misc.align *)
(*s: signature Misc.no_overflow_add *)
val no_overflow_add: int -> int -> bool
        (* [no_overflow_add n1 n2] returns [true] if the computation of
           [n1 + n2] does not overflow. *)
(*e: signature Misc.no_overflow_add *)
(*s: signature Misc.no_overflow_sub *)
val no_overflow_sub: int -> int -> bool
        (* [no_overflow_add n1 n2] returns [true] if the computation of
           [n1 - n2] does not overflow. *)
(*e: signature Misc.no_overflow_sub *)
(*e: ./utils/misc.mli *)
