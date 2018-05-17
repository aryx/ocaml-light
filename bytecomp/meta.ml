(*s: bytecomp/meta.ml *)
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

external global_data : unit -> Obj.t array = "get_global_data"
external realloc_global_data : int -> unit = "realloc_global"
external static_alloc : int -> string = "static_alloc"
external static_free : string -> unit = "static_free"
(*s: type Meta.closure (./bytecomp/meta.ml) *)
type closure = unit -> Obj.t
(*e: type Meta.closure (./bytecomp/meta.ml) *)
external reify_bytecode : string -> int -> closure = "reify_bytecode"
external available_primitives : unit -> string array = "available_primitives"
(*e: bytecomp/meta.ml *)
