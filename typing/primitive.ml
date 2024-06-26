(*s: typing/primitive.ml *)
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

(* Description of primitive functions *)

open Format

(*s: type [[Primitive.description]] *)
(* Description of primitive functions *)

type description =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)

    prim_native_float: bool }  (* Does the above operate on unboxed floats? *)
(*e: type [[Primitive.description]] *)

(*s: function [[Primitive.parse_declaration]] *)
let parse_declaration arity decl =
  match decl with
    name :: "noalloc" :: name2 :: "float" :: _ ->
      Some{prim_name = name; prim_arity = arity; prim_alloc = false;
           prim_native_name = name2; prim_native_float = true}
  | name :: "noalloc" :: name2 :: _ ->
      Some{prim_name = name; prim_arity = arity; prim_alloc = false;
           prim_native_name = name2; prim_native_float = false}
  | name :: name2 :: "float" :: _ ->
      Some{prim_name = name; prim_arity = arity; prim_alloc = true;
           prim_native_name = name2; prim_native_float = true}
  | name :: "noalloc" :: _ ->
      Some{prim_name = name; prim_arity = arity; prim_alloc = false;
           prim_native_name = ""; prim_native_float = false}
  | name :: name2 :: _ ->
      Some{prim_name = name; prim_arity = arity; prim_alloc = true;
           prim_native_name = name2; prim_native_float = false}
  | name :: _ ->
      Some{prim_name = name; prim_arity = arity; prim_alloc = true;
           prim_native_name = ""; prim_native_float = false}
  | [] ->
      None
(*e: function [[Primitive.parse_declaration]] *)

(*s: function [[Primitive.print_quoted]] *)
let print_quoted s = print_char '"'; print_string s; print_char '"'
(*e: function [[Primitive.print_quoted]] *)

(*s: function [[Primitive.print_description]] *)
let print_description p =
  print_quoted p.prim_name;
  if not p.prim_alloc then
    (print_space(); print_quoted "noalloc");
  if p.prim_native_name <> "" then
    (print_space(); print_quoted p.prim_native_name);
  if p.prim_native_float then
    (print_space(); print_quoted "float")
(*e: function [[Primitive.print_description]] *)
(*e: typing/primitive.ml *)
