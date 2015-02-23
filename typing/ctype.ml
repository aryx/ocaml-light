(*s: ./typing/ctype.ml *)
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

(* $Id$ *)

(* Operations on core types *)

open Misc
open Path
open Types

(*s: exception Ctype.Unify (./typing/ctype.ml) *)
exception Unify
(*e: exception Ctype.Unify (./typing/ctype.ml) *)

(*s: constant Ctype.current_level *)
let current_level = ref 0
(*e: constant Ctype.current_level *)

(*s: constant Ctype.generic_level *)
let generic_level = (-1)
(*e: constant Ctype.generic_level *)

(*s: function Ctype.reset_def *)
let reset_def () = current_level := 0
(*e: function Ctype.reset_def *)
(*s: function Ctype.begin_def *)
let begin_def () = incr current_level
(*e: function Ctype.begin_def *)
(*s: function Ctype.end_def *)
let end_def () = decr current_level
(*e: function Ctype.end_def *)

(*s: function Ctype.newvar *)
let newvar () =
  Tvar { tvar_level = !current_level; tvar_link = None }
(*e: function Ctype.newvar *)

(*s: function Ctype.new_global_var *)
let new_global_var () =
  Tvar { tvar_level = 1; tvar_link = None }
(*e: function Ctype.new_global_var *)

(*s: function Ctype.repr *)
let rec repr = function
    Tvar({tvar_link = Some ty} as v) ->
      let r = repr ty in
      if r != ty then v.tvar_link <- Some r;
      r
  | t -> t
(*e: function Ctype.repr *)

(*s: constant Ctype.none *)
let none = Ttuple []                  (* Clearly ill-formed type *)
(*e: constant Ctype.none *)

(*s: function Ctype.generalize *)
(* Type generalization *)

let rec generalize ty =
  match repr ty with
    Tvar v ->
      if v.tvar_level > !current_level then v.tvar_level <- generic_level
  | Tarrow(t1, t2) ->
      generalize t1; generalize t2
  | Ttuple tl ->
      List.iter generalize tl
  | Tconstr(p, []) ->
      ()
  | Tconstr(p, tl) ->
      List.iter generalize tl
(*e: function Ctype.generalize *)

(*s: function Ctype.make_nongen *)
let rec make_nongen ty =
  match repr ty with
    Tvar v ->
      if v.tvar_level > !current_level then v.tvar_level <- !current_level
  | Tarrow(t1, t2) ->
      make_nongen t1; make_nongen t2
  | Ttuple tl ->
      List.iter make_nongen tl
  | Tconstr(p, []) ->
      ()
  | Tconstr(p, tl) ->
      List.iter make_nongen tl
(*e: function Ctype.make_nongen *)

(*s: constant Ctype.inst_subst *)
(* Taking instances of type schemes *)

let inst_subst = ref ([] : (type_expr * type_expr) list)
(*e: constant Ctype.inst_subst *)

(*s: function Ctype.copy *)
let rec copy ty =
  match repr ty with
    Tvar v as t ->
      if v.tvar_level = generic_level then begin
        try
          List.assq t !inst_subst
        with Not_found ->
          let t' = newvar() in
          inst_subst := (t, t') :: !inst_subst;
          t'
      end else t
  | Tarrow(t1, t2) ->
      Tarrow(copy t1, copy t2)
  | Ttuple tl ->
      Ttuple(List.map copy tl)
  | Tconstr(p, []) as t ->
      t
  | Tconstr(p, tl) ->
      Tconstr(p, List.map copy tl)
(*e: function Ctype.copy *)

(*s: function Ctype.instance *)
let instance sch =
  inst_subst := [];
  let ty = copy sch in
  inst_subst := [];
  ty
(*e: function Ctype.instance *)

(*s: function Ctype.instance_constructor *)
let instance_constructor cstr =
  inst_subst := [];
  let ty_res = copy cstr.cstr_res in
  let ty_args = List.map copy cstr.cstr_args in
  inst_subst := [];
  (ty_args, ty_res)
(*e: function Ctype.instance_constructor *)

(*s: function Ctype.instance_label *)
let instance_label lbl =
  inst_subst := [];
  let ty_res = copy lbl.lbl_res in
  let ty_arg = copy lbl.lbl_arg in
  inst_subst := [];
  (ty_arg, ty_res)
(*e: function Ctype.instance_label *)

(*s: function Ctype.substitute *)
let substitute params args body =
  inst_subst := List.combine params args;
  let ty = copy body in
  inst_subst := [];
  ty
(*e: function Ctype.substitute *)

(*s: exception Ctype.Cannot_expand *)
(* Unification *)

exception Cannot_expand
(*e: exception Ctype.Cannot_expand *)

(*s: function Ctype.expand_abbrev *)
let expand_abbrev env path args =
  try
    let decl = Env.find_type path env in
    match decl.type_manifest with
      Some body -> substitute decl.type_params args body
    | None -> raise Cannot_expand
  with Not_found ->
    raise Cannot_expand
(*e: function Ctype.expand_abbrev *)

(*s: function Ctype.occur *)
let rec occur tvar ty =
  match repr ty with
    Tvar v ->
      if v == tvar then raise Unify;
      if v.tvar_level > tvar.tvar_level then v.tvar_level <- tvar.tvar_level
  | Tarrow(t1, t2) ->
      occur tvar t1; occur tvar t2
  | Ttuple tl ->
      List.iter (occur tvar) tl
  | Tconstr(p, []) ->
      ()
  | Tconstr(p, tl) ->
      List.iter (occur tvar) tl
(*e: function Ctype.occur *)

let rec unify env t1 t2 =
  if t1 == t2 then () else begin
    let t1 = repr t1 in
    let t2 = repr t2 in
    if t1 == t2 then () else begin
      match (t1, t2) with
        (Tvar v, _) ->
          occur v t2; v.tvar_link <- Some t2
      | (_, Tvar v) ->
          occur v t1; v.tvar_link <- Some t1
      | (Tarrow(t1, u1), Tarrow(t2, u2)) ->
          unify env t1 t2; unify env u1 u2
      | (Ttuple tl1, Ttuple tl2) ->
          unify_list env tl1 tl2
      | (Tconstr(p1, tl1), Tconstr(p2, tl2)) ->
          if Path.same p1 p2 then
            unify_list env tl1 tl2
          else begin
            try
              unify env (expand_abbrev env p1 tl1) t2
            with Cannot_expand ->
            try
              unify env t1 (expand_abbrev env p2 tl2)
            with Cannot_expand ->
              raise Unify
          end
      | (Tconstr(p1, tl1), _) ->
          begin try
            unify env (expand_abbrev env p1 tl1) t2
          with Cannot_expand ->
            raise Unify
          end
      | (_, Tconstr(p2, tl2)) ->
          begin try
            unify env t1 (expand_abbrev env p2 tl2)
          with Cannot_expand ->
            raise Unify
          end
      | (_, _) ->
          raise Unify
    end
  end

and unify_list env tl1 tl2 =
  match (tl1, tl2) with
    ([], []) -> ()
  | (t1::r1, t2::r2) -> unify env t1 t2; unify_list env r1 r2
  | (_, _) -> raise Unify

(*s: function Ctype.filter_arrow *)
let rec filter_arrow env t =
  match repr t with
    Tvar v ->
      let t1 = Tvar { tvar_level = v.tvar_level; tvar_link = None }
      and t2 = Tvar { tvar_level = v.tvar_level; tvar_link = None } in
      v.tvar_link <- Some(Tarrow(t1, t2));
      (t1, t2)
  | Tarrow(t1, t2) ->
      (t1, t2)
  | Tconstr(p, tl) ->
      begin try
        filter_arrow env (expand_abbrev env p tl)
      with Cannot_expand ->
        raise Unify
      end
  | _ ->
      raise Unify
(*e: function Ctype.filter_arrow *)

(*s: function Ctype.moregen_occur *)
(* Matching between type schemes *)

let rec moregen_occur tvar ty =
  match repr ty with
    Tvar v ->
      if v == tvar then raise Unify;
      (* tvar has level = !current_level iff it is generic
         in the original type scheme. In this case, it can be freely
         instantiated. Otherwise, tvar is not generic
         and cannot be instantiated by a type that contains
         generic variables. *)
      if v.tvar_level = generic_level & tvar.tvar_level < !current_level
      then raise Unify
  | Tarrow(t1, t2) ->
      moregen_occur tvar t1; moregen_occur tvar t2
  | Ttuple tl ->
      List.iter (moregen_occur tvar) tl
  | Tconstr(p, []) ->
      ()
  | Tconstr(p, tl) ->
      List.iter (moregen_occur tvar) tl
(*e: function Ctype.moregen_occur *)

let rec moregen env t1 t2 =
  if t1 == t2 then () else begin
    let t1 = repr t1 in
    let t2 = repr t2 in
    if t1 == t2 then () else begin
      match (t1, t2) with
        (Tvar v, _) ->
          if v.tvar_level = generic_level then raise Unify;
          moregen_occur v t2;
          v.tvar_link <- Some t2
      | (Tarrow(t1, u1), Tarrow(t2, u2)) ->
          moregen env t1 t2; moregen env u1 u2
      | (Ttuple tl1, Ttuple tl2) ->
          moregen_list env tl1 tl2
      | (Tconstr(p1, tl1), Tconstr(p2, tl2)) ->
          if Path.same p1 p2 then
            moregen_list env tl1 tl2
          else begin
            try
              moregen env (expand_abbrev env p1 tl1) t2
            with Cannot_expand ->
            try
              moregen env t1 (expand_abbrev env p2 tl2)
            with Cannot_expand ->
              raise Unify
          end
      | (Tconstr(p1, tl1), _) ->
          begin try
            moregen env (expand_abbrev env p1 tl1) t2
          with Cannot_expand ->
            raise Unify
          end
      | (_, Tconstr(p2, tl2)) ->
          begin try
            moregen env t1 (expand_abbrev env p2 tl2)
          with Cannot_expand ->
            raise Unify
          end
      | (_, _) ->
          raise Unify
    end
  end

and moregen_list env tl1 tl2 =
  match (tl1, tl2) with
    ([], []) -> ()
  | (t1::r1, t2::r2) -> moregen env t1 t2; moregen_list env r1 r2
  | (_, _) -> raise Unify
  
(*s: function Ctype.moregeneral *)
let moregeneral env sch1 sch2 =
  begin_def();
  try
    moregen env (instance sch1) sch2;
    end_def();
    true
  with Unify ->
    end_def();
    false
(*e: function Ctype.moregeneral *)

(*s: function Ctype.equal *)
(* Equivalence between parameterized types *)

let equal env params1 ty1 params2 ty2 =
  let subst = List.combine params1 params2 in
  let rec eqtype t1 t2 =
    let t1 = repr t1 in
    let t2 = repr t2 in
    match (t1, t2) with
      (Tvar _, Tvar _) ->
        begin try
          List.assq t1 subst == t2
        with Not_found ->
          fatal_error "Ctype.equal"
        end
    | (Tarrow(t1, u1), Tarrow(t2, u2)) ->
        eqtype t1 t2 & eqtype u1 u2
    | (Ttuple tl1, Ttuple tl2) ->
        eqtype_list tl1 tl2
    | (Tconstr(p1, tl1), Tconstr(p2, tl2)) ->
        if Path.same p1 p2 then
          eqtype_list tl1 tl2
        else begin
          try
            eqtype (expand_abbrev env p1 tl1) t2
          with Cannot_expand ->
          try
            eqtype t1 (expand_abbrev env p2 tl2)
          with Cannot_expand ->
            false
        end
    | (Tconstr(p1, tl1), _) ->
        begin try
          eqtype (expand_abbrev env p1 tl1) t2
        with Cannot_expand ->
          false
        end
    | (_, Tconstr(p2, tl2)) ->
        begin try
          eqtype t1 (expand_abbrev env p2 tl2)
        with Cannot_expand ->
          false
        end
    | (_, _) ->
        false
  and eqtype_list tl1 tl2 =
    match (tl1, tl2) with
      ([], []) -> true
    | (t1::r1, t2::r2) -> eqtype t1 t2 & eqtype_list r1 r2
    | (_, _) -> false
  in
    eqtype ty1 ty2
(*e: function Ctype.equal *)

(*s: function Ctype.nondep_type *)
(* Remove dependencies *)

let rec nondep_type env id ty =
  match repr ty with
    Tvar v as tvar -> tvar
  | Tarrow(t1, t2) ->
      Tarrow(nondep_type env id t1, nondep_type env id t2)
  | Ttuple tl ->
      Ttuple(List.map (nondep_type env id) tl)
  | Tconstr(p, tl) ->
      if Path.isfree id p then begin
        let ty' =
          try
            expand_abbrev env p tl
          with Cannot_expand ->
            raise Not_found in
        nondep_type env id ty'
      end else
        Tconstr(p, List.map (nondep_type env id) tl)
(*e: function Ctype.nondep_type *)

(*s: function Ctype.free_type_ident *)
let rec free_type_ident env ids ty =
  match repr ty with
    Tvar _ -> false
  | Tarrow(t1, t2) ->
      free_type_ident env ids t1 or free_type_ident env ids t2
  | Ttuple tl ->
      List.exists (free_type_ident env ids) tl
  | Tconstr((Pident id as p), tl) ->
      List.exists (Ident.same id) ids or begin
        try
          free_type_ident env (id::ids) (expand_abbrev env p tl)
        with Cannot_expand ->
          List.exists (free_type_ident env ids) tl
      end
  | Tconstr(p, tl) ->
      begin try
        free_type_ident env ids (expand_abbrev env p tl)
      with Cannot_expand ->
        List.exists (free_type_ident env ids) tl
      end
(*e: function Ctype.free_type_ident *)

(*s: function Ctype.closed_schema *)
(* Miscellaneous *)

let rec closed_schema ty =
  match repr ty with
    Tvar v -> v.tvar_level = generic_level
  | Tarrow(t1, t2) -> closed_schema t1 & closed_schema t2
  | Ttuple tl -> List.for_all closed_schema tl
  | Tconstr(p, tl) -> List.for_all closed_schema tl
(*e: function Ctype.closed_schema *)

(*s: function Ctype.is_generic *)
let is_generic ty =
  match repr ty with
    Tvar v -> v.tvar_level = generic_level
  | _ -> fatal_error "Ctype.is_generic"
(*e: function Ctype.is_generic *)

(*s: function Ctype.arity *)
let rec arity ty =
  match repr ty with
    Tarrow(t1, t2) -> 1 + arity t2
  | _ -> 0
(*e: function Ctype.arity *)

(*s: function Ctype.newty *)
let newty x = x
(*e: function Ctype.newty *)
(*s: function Ctype.init_def *)
let init_def _ = ()
(*e: function Ctype.init_def *)
(*e: ./typing/ctype.ml *)
