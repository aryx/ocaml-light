(*s: ./typing/ctype.mli *)
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

(*s: exception Ctype.Unify *)
exception Unify of (type_expr * type_expr) list
(*e: exception Ctype.Unify *)
(*s: exception Ctype.Subtype *)
exception Subtype of
        (type_expr * type_expr) list * (type_expr * type_expr) list
(*e: exception Ctype.Subtype *)
(*s: exception Ctype.Cannot_expand *)
exception Cannot_expand
(*e: exception Ctype.Cannot_expand *)
(*s: exception Ctype.Cannot_apply *)
exception Cannot_apply
(*e: exception Ctype.Cannot_apply *)
(*s: exception Ctype.Recursive_abbrev *)
exception Recursive_abbrev
(*e: exception Ctype.Recursive_abbrev *)

(*s: signature Ctype.init_def *)
val init_def: int -> unit
        (* Set the initial variable level *)
(*e: signature Ctype.init_def *)
(*s: signature Ctype.begin_def *)
val begin_def: unit -> unit
        (* Raise the variable level by one at the beginning of a definition. *)
(*e: signature Ctype.begin_def *)
(*s: signature Ctype.end_def *)
val end_def: unit -> unit
        (* Lower the variable level by one at the end of a definition *)
(*e: signature Ctype.end_def *)
(*s: signature Ctype.reset_global_level *)
val reset_global_level: unit -> unit
(*e: signature Ctype.reset_global_level *)

(*s: signature Ctype.newty *)
val newty: type_desc -> type_expr
(*e: signature Ctype.newty *)
(*s: signature Ctype.newgenty *)
val newgenty: type_desc -> type_expr
(*e: signature Ctype.newgenty *)
(*s: signature Ctype.newvar *)
val newvar: unit -> type_expr
        (* Return a fresh variable *)
(*e: signature Ctype.newvar *)
(*s: signature Ctype.new_global_var *)
val new_global_var: unit -> type_expr
        (* Return a fresh variable, bound at toplevel
           (as type variables ['a] in type constraints). *)
(*e: signature Ctype.new_global_var *)
(*s: signature Ctype.none *)
val none: type_expr
        (* A dummy type expression *)
(*e: signature Ctype.none *)

(*s: signature Ctype.repr *)
val repr: type_expr -> type_expr
        (* Return the canonical representative of a type. *)
(*e: signature Ctype.repr *)

(*s: signature Ctype.generalize *)
val generalize: type_expr -> unit
        (* Generalize in-place the given type *)
(*e: signature Ctype.generalize *)
(*s: signature Ctype.make_nongen *)
val make_nongen: type_expr -> unit
        (* Make non-generalizable the given type *)
(*e: signature Ctype.make_nongen *)
(*s: signature Ctype.correct_levels *)
val correct_levels: type_expr -> type_expr
        (* Returns a copy with decreasing levels *)
(*e: signature Ctype.correct_levels *)

(*s: signature Ctype.instance *)
val instance: type_expr -> type_expr
        (* Take an instance of a type scheme *)
(*e: signature Ctype.instance *)
(*s: signature Ctype.instance_list *)
val instance_list: type_expr list -> type_expr list
        (* Take an instance of a list of type schemes *)
(*e: signature Ctype.instance_list *)
(*s: signature Ctype.instance_constructor *)
val instance_constructor:
        constructor_description -> type_expr list * type_expr
        (* Same, for a constructor *)
(*e: signature Ctype.instance_constructor *)
(*s: signature Ctype.instance_label *)
val instance_label: label_description -> type_expr * type_expr
        (* Same, for a label *)
(*e: signature Ctype.instance_label *)
(*s: signature Ctype.instance_parameterized_type *)
val instance_parameterized_type:
        type_expr list -> type_expr -> type_expr list * type_expr
(*e: signature Ctype.instance_parameterized_type *)
(*s: signature Ctype.apply *)
val apply:
        Env.t -> type_expr list -> type_expr -> type_expr list -> type_expr
        (* [apply [p1...pN] t [a1...aN]] match the arguments [ai] to
        the parameters [pi] and returns the corresponding instance of
        [t]. Exception [Cannot_apply] is raised in case of failure. *)
(*e: signature Ctype.apply *)

(* val expand_abbrev: *)
(*         Env.t -> Path.t -> type_expr list -> Types.abbrev_memo ref -> *)
(*         int -> type_expr *)
(*s: signature Ctype.expand_head *)
(*         (* Expand an abbreviation *) *)
val expand_head: Env.t -> type_expr -> type_expr
(*e: signature Ctype.expand_head *)
(*s: signature Ctype.full_expand *)
val full_expand: Env.t -> type_expr -> type_expr
(*e: signature Ctype.full_expand *)

(*s: signature Ctype.unify *)
val unify: Env.t -> type_expr -> type_expr -> unit
        (* Unify the two types given. Raise [Unify] if not possible. *)
(*e: signature Ctype.unify *)
(*s: signature Ctype.filter_arrow *)
val filter_arrow: Env.t -> type_expr -> type_expr * type_expr
        (* A special case of unification (with 'a -> 'b). *)
(*e: signature Ctype.filter_arrow *)

(*s: signature Ctype.moregeneral *)
val moregeneral: Env.t -> bool -> type_expr -> type_expr -> bool
        (* Check if the first type scheme is more general than the second. *)
(*e: signature Ctype.moregeneral *)

(*s: signature Ctype.equal *)
val equal: Env.t -> bool -> type_expr list -> type_expr list -> bool
        (* [equal env [x1...xn] tau [y1...yn] sigma]
           checks whether the parameterized types
           [/\x1.../\xn.tau] and [/\y1.../\yn.sigma] are equivalent. *)
(*e: signature Ctype.equal *)

(*s: signature Ctype.enlarge_type *)
val enlarge_type: Env.t -> type_expr -> type_expr
        (* Make a type larger *)
(*e: signature Ctype.enlarge_type *)
(*s: signature Ctype.subtype *)
val subtype : Env.t -> type_expr -> type_expr -> unit -> unit
        (* [subtype env t1 t2] checks that [t1] is a subtype of [t2].
           It accumulates the constraints the type variables must
           enforce and returns a function that inforce this
           constraints. *)
(*e: signature Ctype.subtype *)

(*s: signature Ctype.nondep_type *)
val nondep_type: Env.t -> Ident.t -> type_expr -> type_expr
        (* Return a type equivalent to the given type but without
           references to the given module identifier. Raise [Not_found]
           if no such type exists. *)
(*e: signature Ctype.nondep_type *)
(*s: signature Ctype.nondep_type_decl *)
val nondep_type_decl:
        Env.t -> Ident.t -> Ident.t -> bool -> type_declaration ->
        type_declaration
        (* Same for type declarations. *)
(*e: signature Ctype.nondep_type_decl *)

(*s: signature Ctype.correct_abbrev *)
val correct_abbrev: Env.t -> Ident.t -> type_expr list -> type_expr -> unit
(*e: signature Ctype.correct_abbrev *)
(*s: signature Ctype.cyclic_abbrev *)
val cyclic_abbrev: Env.t -> Ident.t -> type_expr -> bool
(*e: signature Ctype.cyclic_abbrev *)

(*s: type Ctype.closed_schema_result *)
type closed_schema_result = Var of type_expr | Row_var of type_expr
(*e: type Ctype.closed_schema_result *)
(*s: signature Ctype.closed_schema *)
val closed_schema: type_expr -> bool
(*e: signature Ctype.closed_schema *)

(*s: signature Ctype.unalias *)
val unalias: type_expr -> type_expr
(*e: signature Ctype.unalias *)
(*s: signature Ctype.unroll_abbrev *)
val unroll_abbrev: Ident.t -> type_expr list -> type_expr -> type_expr
(*e: signature Ctype.unroll_abbrev *)
(*s: signature Ctype.arity *)
val arity: type_expr -> int
        (* Return the arity (as for curried functions) of the given type. *)
(*e: signature Ctype.arity *)
(*e: ./typing/ctype.mli *)
