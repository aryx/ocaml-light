(*s: ./typing/ctype.mli *)
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

(* Operations on core types *)

open Types

(*s: signature Ctype.begin_def *)
val begin_def: unit -> unit
        (* Raise the variable level by one at the beginning of a definition. *)
(*e: signature Ctype.begin_def *)
(*s: signature Ctype.end_def *)
val end_def: unit -> unit
        (* Lower the variable level by one at the end of a definition *)
(*e: signature Ctype.end_def *)
(*s: signature Ctype.reset_def *)
val reset_def: unit -> unit
        (* Reset (to 0) the variable level *)
(*e: signature Ctype.reset_def *)
(*s: signature Ctype.newvar *)
val newvar: unit -> type_expr
        (* Return a fresh variable *)
(*e: signature Ctype.newvar *)
(*s: signature Ctype.new_global_var *)
val new_global_var: unit -> type_expr
        (* Return a fresh variable, bound at toplevel
           (as type variables ['a] in type constraints). *)
(*e: signature Ctype.new_global_var *)
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
(*s: signature Ctype.instance *)
val instance: type_expr -> type_expr
        (* Take an instance of a type scheme *)
(*e: signature Ctype.instance *)
(*s: signature Ctype.instance_constructor *)
val instance_constructor: constructor_description -> type_expr list * type_expr
        (* Same, for a constructor *)
(*e: signature Ctype.instance_constructor *)
(*s: signature Ctype.instance_label *)
val instance_label: label_description -> type_expr * type_expr
        (* Same, for a label *)
(*e: signature Ctype.instance_label *)
(*s: signature Ctype.unify *)
val unify: Env.t -> type_expr -> type_expr -> unit
        (* Unify the two types given. Raise [Unify] if not possible. *)
(*e: signature Ctype.unify *)
(*s: signature Ctype.filter_arrow *)
val filter_arrow: Env.t -> type_expr -> type_expr * type_expr
        (* A special case of unification (with 'a -> 'b). *)
(*e: signature Ctype.filter_arrow *)
(*s: signature Ctype.moregeneral *)
val moregeneral: Env.t -> type_expr -> type_expr -> bool
        (* Check if the first type scheme is more general than the second. *)
(*e: signature Ctype.moregeneral *)
(*s: signature Ctype.equal *)
val equal: Env.t -> type_expr list -> type_expr ->
                       type_expr list -> type_expr -> bool
        (* [equal env [x1...xn] tau [y1...yn] sigma]
           checks whether the parameterized types
           [/\x1.../\xn.tau] and [/\y1.../\yn.sigma] are equivalent. *)
(*e: signature Ctype.equal *)
(*s: signature Ctype.closed_schema *)
val closed_schema: type_expr -> bool
        (* Check whether the given type scheme contains no non-generic
           type variables *)
(*e: signature Ctype.closed_schema *)
(*s: signature Ctype.nondep_type *)
val nondep_type: Env.t -> Ident.t -> type_expr -> type_expr
        (* Return a type equivalent to the given type but without
           references to the given module identifier. Raise [Not_found]
           if no such type exists. *)
(*e: signature Ctype.nondep_type *)
(*s: signature Ctype.free_type_ident *)
val free_type_ident: Env.t -> Ident.t list -> type_expr -> bool
        (* Test whether one of the given type identifiers occur free
           in the given type expression. *)
(*e: signature Ctype.free_type_ident *)
(*s: signature Ctype.is_generic *)
val is_generic: type_expr -> bool
        (* Test whether the given type variable is generic *)
(*e: signature Ctype.is_generic *)
(*s: signature Ctype.arity *)
val arity: type_expr -> int
        (* Return the arity (as for curried functions) of the given type. *)
(*e: signature Ctype.arity *)
(*s: signature Ctype.none *)
val none: type_expr
        (* A dummy type expression *)
(*e: signature Ctype.none *)
(*s: signature Ctype.substitute *)
val substitute:
        type_expr list -> type_expr list -> type_expr -> type_expr
        (* [substitute [v1...vN] [t1...tN] t]
           returns a copy of [t] where the [vi] are replaced
           by the [ti]. *)
(*e: signature Ctype.substitute *)

(*s: exception Ctype.Unify *)
exception Unify
(*e: exception Ctype.Unify *)

(*e: ./typing/ctype.mli *)
