(*s: ./typing/primitive.mli *)
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


(*s: type [[Primitive.description]] *)
(* Description of primitive functions *)

type description =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)

    prim_native_float: bool }  (* Does the above operate on unboxed floats? *)
(*e: type [[Primitive.description]] *)

(*s: signature [[Primitive.parse_declaration]] *)
val parse_declaration: int -> string list -> description option
(*e: signature [[Primitive.parse_declaration]] *)
(*s: signature [[Primitive.print_description]] *)
val print_description: description -> unit
(*e: signature [[Primitive.print_description]] *)
(*e: ./typing/primitive.mli *)
