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

(* $Id: ctype.mli,v 1.28 1997/06/29 13:16:46 vouillon Exp $ *)

(* Operations on core types *)

open Asttypes
open Types

exception Unify of (type_expr * type_expr) list
exception Subtype of
        (type_expr * type_expr) list * (type_expr * type_expr) list
exception Cannot_expand
exception Cannot_apply
exception Recursive_abbrev

val init_def: int -> unit
        (* Set the initial variable level *)
val begin_def: unit -> unit
        (* Raise the variable level by one at the beginning of a definition. *)
val end_def: unit -> unit
        (* Lower the variable level by one at the end of a definition *)
val reset_global_level: unit -> unit

val newty: type_desc -> type_expr
val newgenty: type_desc -> type_expr
val newvar: unit -> type_expr
        (* Return a fresh variable *)
val new_global_var: unit -> type_expr
        (* Return a fresh variable, bound at toplevel
           (as type variables ['a] in type constraints). *)
val none: type_expr
        (* A dummy type expression *)

val repr: type_expr -> type_expr
        (* Return the canonical representative of a type. *)

val opened_object: type_expr -> bool
val close_object: type_expr -> unit
val set_object_name:
        type_expr -> type_expr list -> Ident.t -> unit
val remove_object_name: type_expr -> unit
val hide_private_methods: type_expr -> unit

val generalize: type_expr -> unit
        (* Generalize in-place the given type *)
val make_nongen: type_expr -> unit
        (* Make non-generalizable the given type *)
val correct_levels: type_expr -> type_expr
        (* Returns a copy with decreasing levels *)

val instance: type_expr -> type_expr
        (* Take an instance of a type scheme *)
val instance_list: type_expr list -> type_expr list
        (* Take an instance of a list of type schemes *)
val instance_constructor:
        constructor_description -> type_expr list * type_expr
        (* Same, for a constructor *)
val instance_label: label_description -> type_expr * type_expr
        (* Same, for a label *)
val instance_parameterized_type:
        type_expr list -> type_expr -> type_expr list * type_expr
val instance_parameterized_type_2:
        type_expr list -> type_expr list -> type_expr ->
        type_expr list * type_expr list * type_expr
val apply:
        Env.t -> type_expr list -> type_expr -> type_expr list -> type_expr
        (* [apply [p1...pN] t [a1...aN]] match the arguments [ai] to
        the parameters [pi] and returns the corresponding instance of
        [t]. Exception [Cannot_apply] is raised in case of failure. *)

(* val expand_abbrev: *)
(*         Env.t -> Path.t -> type_expr list -> Types.abbrev_memo ref -> *)
(*         int -> type_expr *)
(*         (* Expand an abbreviation *) *)
val expand_head: Env.t -> type_expr -> type_expr
val full_expand: Env.t -> type_expr -> type_expr

val unify: Env.t -> type_expr -> type_expr -> unit
        (* Unify the two types given. Raise [Unify] if not possible. *)
val filter_arrow: Env.t -> type_expr -> type_expr * type_expr
        (* A special case of unification (with 'a -> 'b). *)

val moregeneral: Env.t -> bool -> type_expr -> type_expr -> bool
        (* Check if the first type scheme is more general than the second. *)

val equal: Env.t -> bool -> type_expr list -> type_expr list -> bool
        (* [equal env [x1...xn] tau [y1...yn] sigma]
           checks whether the parameterized types
           [/\x1.../\xn.tau] and [/\y1.../\yn.sigma] are equivalent. *)

val enlarge_type: Env.t -> type_expr -> type_expr
        (* Make a type larger *)
val subtype : Env.t -> type_expr -> type_expr -> unit -> unit
        (* [subtype env t1 t2] checks that [t1] is a subtype of [t2].
           It accumulates the constraints the type variables must
           enforce and returns a function that inforce this
           constraints. *)

val nondep_type: Env.t -> Ident.t -> type_expr -> type_expr
        (* Return a type equivalent to the given type but without
           references to the given module identifier. Raise [Not_found]
           if no such type exists. *)
val nondep_type_decl:
        Env.t -> Ident.t -> Ident.t -> bool -> type_declaration ->
        type_declaration
        (* Same for type declarations. *)

val correct_abbrev: Env.t -> Ident.t -> type_expr list -> type_expr -> unit
val cyclic_abbrev: Env.t -> Ident.t -> type_expr -> bool

type closed_schema_result = Var of type_expr | Row_var of type_expr
val closed_schema: type_expr -> bool
val closed_schema_verbose: type_expr -> closed_schema_result option
        (* Check whether the given type scheme contains no non-generic
           type variables *)

val unalias: type_expr -> type_expr
val unroll_abbrev: Ident.t -> type_expr list -> type_expr -> type_expr
val arity: type_expr -> int
        (* Return the arity (as for curried functions) of the given type. *)
