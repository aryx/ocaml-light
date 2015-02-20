(*s: ./typing/ctype.ml *)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


(* Operations on core types *)

open Misc
open Asttypes
open Types
open Btype

(*
   Type manipulation after type inference
   ======================================
   If one wants to manipulate a type after type inference (for
   instance, during code generation or in the debugger), one must
   first make sure that the type levels are correct, using the
   function [correct_levels]. Then, this type can be correctely
   manipulated by [apply], [expand_head] and [moregeneral].
*)

(*
   General notes
   =============
   - As much sharing as possible should be kept : it makes types
     smaller and better abbreviated.
     When necessary, some sharing can be lost. Types will still be
     printed correctly (+++ TO DO...), and abbreviations defined by a
     class do not depend on sharing thanks to constrained
     abbreviations. (Of course, even if some sharing is lost, typing
     will still be correct.)
   - All nodes of a type have a level : that way, one know whether a
     node need to be duplicated or not when instantiating a type.
   - Levels of a type are decreasing (generic level being considered
     as greatest).
   - The level of a type constructor is superior to the binding
     time of its path.
   - Recursive types without limitation should be handled (even if
     there is still an occur check). This avoid treating specially the
     case for objects, for instance. Furthermore, the occur check
     policy can then be easily changed.
*)

(*
   A faire
   =======
   - Revoir affichage des types.
   - Etendre la portee d'un alias [... as 'a] a tout le type englobant.
   - #-type implementes comme de vraies abreviations.
   - Niveaux plus fins pour les identificateurs :
       Champ [global] renomme en [level];
       Niveau -1 : global
               0 : module toplevel
               1 : module contenu dans module toplevel
              ...
     En fait, incrementer le niveau a chaque fois que l'on rentre dans
     un module.

       3   4 6
        \ / /
       1 2 5
        \|/
         0

     [Subst] doit ecreter les niveaux (pour qu'un variable non
     generalisable dans un module de niveau 2 ne se retrouve pas
     generalisable lorsque l'on l'utilise au niveau 0).

   - Traitement de la trace de l'unification separe de la fonction
     [unify].
*)

(*s: exception Ctype.Unify *)
(**** Errors ****)

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

(*s: constant Ctype.current_level *)
(**** Type level management ****)

let current_level = ref 0
(*e: constant Ctype.current_level *)
(*s: constant Ctype.global_level *)
let global_level = ref 1
(*e: constant Ctype.global_level *)

(*s: function Ctype.init_def *)
let init_def level = current_level := level
(*e: function Ctype.init_def *)
(*s: function Ctype.begin_def *)
let begin_def () = incr current_level
(*e: function Ctype.begin_def *)
(*s: function Ctype.end_def *)
let end_def () = decr current_level
(*e: function Ctype.end_def *)

(*s: function Ctype.reset_global_level *)
let reset_global_level () =
  global_level := !current_level + 1
(*e: function Ctype.reset_global_level *)

(**** Some type creators ****)

(*s: function Ctype.newty *)
(* Re-export generic type creators *)

let newty desc         = { desc = desc; level = !current_level }
(*e: function Ctype.newty *)
(*s: constant Ctype.newgenty *)
let newgenty           = newgenty
(*e: constant Ctype.newgenty *)
(*s: function Ctype.new_global_ty *)
let new_global_ty desc = { desc = desc; level = !global_level }
(*e: function Ctype.new_global_ty *)

(*s: function Ctype.newvar *)
let newvar ()          = { desc = Tvar; level = !current_level }
(*e: function Ctype.newvar *)
(*s: function Ctype.newmarkedvar *)
let newmarkedvar () = { desc = Tvar; level = pivot_level - !current_level }
(*e: function Ctype.newmarkedvar *)
(*s: function Ctype.new_global_var *)
let new_global_var ()  = new_global_ty Tvar
(*e: function Ctype.new_global_var *)

(*s: constant Ctype.none *)
let none = newty (Ttuple [])                (* Clearly ill-formed type *)
(*e: constant Ctype.none *)

(**** Representative of a type ****)

(*s: constant Ctype.repr *)
(* Re-export repr *)
let repr = repr
(*e: constant Ctype.repr *)


                         (*****************************)
                         (*  Type level manipulation  *)
                         (*****************************)

(*
   It would be a bit more efficient to remove abbreviation expansions
   rather than generalizing them: these expansions will usually not be
   used anymore. However, this is not possible in the general case, as
   [expand_abbrev] (via [subst]) requires these expansions to be
   preserved. Does it worth duplicating this code ?
*)
let rec generalize ty =
  let ty = repr ty in
  if (ty.level > !current_level) && (ty.level <> generic_level) then begin
    ty.level <- generic_level;
    begin match ty.desc with
      Tconstr (_, _, abbrev) ->
        generalize_expans !abbrev
    | _ -> ()
    end;
    iter_type_expr generalize ty
  end

and generalize_expans =
  function
    Mnil              ->  ()
  | Mcons(_, ty, rem) ->  generalize ty; generalize_expans rem
  | Mlink rem         ->  generalize_expans !rem

(*s: constant Ctype.try_expand_head' *)
let try_expand_head' = (* Forward declaration *)
  ref (fun env ty -> raise Cannot_expand)
(*e: constant Ctype.try_expand_head' *)

(*
   Lower the levels of a type (assume [level] is not
   [generic_level]).
*)
(*s: function Ctype.update_level *)
(*
    The level of a type constructor must be greater than its binding
    time. That way, a type constructor cannot escape the scope of its
    definition, as would be the case in
      let x = ref []
      module M = struct type t let _ = (x : t list ref) end
    (without this constraint, the type system would actually be unsound.)
*)
let rec update_level env level ty =
  let ty = repr ty in
  if ty.level > level then begin
    begin match ty.desc with
      Tconstr(p, tl, abbrev)  when level < Path.binding_time p ->
        (* Try first to replace an abbreviation by its expansion. *)
        begin try
          ty.desc <- Tlink (!try_expand_head' env ty);
          update_level env level ty
        with Cannot_expand ->
          (* +++ Levels should be restored... *)
          raise (Unify [])
        end
    | _ ->
        ty.level <- level;
        iter_type_expr (update_level env level) ty
    end
  end
(*e: function Ctype.update_level *)

(*s: function Ctype.make_nongen *)
(* 
   Function [update_level] will never try to expand an abbreviation in
   this case ([current_level] is greater than the binding time of any
   type constructor path). So, it can be called with the empty
   environnement.
*)
let make_nongen ty = update_level Env.empty !current_level ty
(*e: function Ctype.make_nongen *)

(*s: function Ctype.correct_levels *)
(* Correct the levels of type [ty]. *)
let correct_levels ty =
  Subst.type_expr Subst.identity ty
(*e: function Ctype.correct_levels *)


                              (*******************)
                              (*  Instantiation  *)
                              (*******************)


(*s: constant Ctype.abbreviations *)
(*
   Generic nodes are duplicated, while non-generic nodes are left
   as-is.
   During instantiation, the description of a generic node is first
   replaced by a link to a stub ([Tlink (newmarkedvar ())]). Once the
   copy is made, it replaces the stub.
   After instantiation, the description of generic node, which was
   stored by [save_desc], must be put back, using [cleanup_types].
   Marked on the copy are removed by [unmark].
*)

let abbreviations = ref (ref Mnil)
  (* Abbreviation memorized. *)
(*e: constant Ctype.abbreviations *)

(*s: function Ctype.copy *)
let rec copy ty =
  let ty = repr ty in
  if ty.level <> generic_level then
    ty
  else begin
    let desc = ty.desc in
    save_desc ty desc;
    let t = newmarkedvar () in          (* Stub *)
    ty.desc <- Tlink t;
    t.desc <-
      begin match desc with
        Tvar ->
          Tvar
      | Tarrow (t1, t2) ->
          Tarrow (copy t1, copy t2)
      | Ttuple tl ->
          Ttuple (List.map copy tl)
      | Tconstr (p, tl, _) ->
          (*
             One must allocate a new reference, so that abbrevia-
             tions belonging to different branches of a type are
             independent.
             Moreover, a reference containing a [Mcons] must be
             shared, so that the memorized expansion of an abbrevi-
             ation can be released by changing the content of just
             one reference.
          *)
          Tconstr (p, List.map copy tl,
                   ref (match ! !abbreviations with
                          Mcons _ -> Mlink !abbreviations
                        | abbrev  -> abbrev))
      | Tnil ->
          Tnil
      | Tlink t -> (* Actually unused *)
          Tlink (copy t)
      end;
    t
  end
(*e: function Ctype.copy *)

(*s: function Ctype.instance *)
(**** Variants of instantiations ****)

let instance sch =
  let ty = copy sch in
  cleanup_types ();
  unmark_type ty;
  ty
(*e: function Ctype.instance *)

(*s: function Ctype.instance_list *)
let instance_list schl =
  let tyl = List.map copy schl in
  cleanup_types ();
  List.iter unmark_type tyl;
  tyl
(*e: function Ctype.instance_list *)

(*s: function Ctype.instance_constructor *)
let instance_constructor cstr =
  let ty_res = copy cstr.cstr_res in
  let ty_args = List.map copy cstr.cstr_args in
  cleanup_types ();
  List.iter unmark_type ty_args; unmark_type ty_res;
  (ty_args, ty_res)
(*e: function Ctype.instance_constructor *)

(*s: function Ctype.instance_label *)
let instance_label lbl =
  let ty_res = copy lbl.lbl_res in
  let ty_arg = copy lbl.lbl_arg in
  cleanup_types ();
  unmark_type ty_arg; unmark_type ty_res;
  (ty_arg, ty_res)
(*e: function Ctype.instance_label *)

(*s: function Ctype.instance_parameterized_type *)
let instance_parameterized_type sch_args sch =
  let ty_args = List.map copy sch_args in
  let ty = copy sch in
  cleanup_types ();
  List.iter unmark_type ty_args; unmark_type ty;
  (ty_args, ty)
(*e: function Ctype.instance_parameterized_type *)

(*s: constant Ctype.unify' *)
(**** Instantiation with parameter substitution ****)

let unify' = (* Forward declaration *)
  ref (fun env ty1 ty2 -> raise (Unify []))
(*e: constant Ctype.unify' *)

(*s: function Ctype.subst *)
let rec subst env level abbrev path params args body =
  let old_level = !current_level in
  current_level := level;
  try
    let body0 = newvar () in          (* Stub *)
    begin match path with
      None      -> ()
    | Some path -> memorize_abbrev abbrev path body0
    end;
    abbreviations := abbrev;
    let (params', body') = instance_parameterized_type params body in
    abbreviations := ref Mnil;
    !unify' env body0 body';
    List.iter2 (!unify' env) params' args;
    current_level := old_level;
    body'
  with Unify _ as exn ->
    current_level := old_level;
    raise exn
(*e: function Ctype.subst *)

(*s: function Ctype.apply *)
(*
   Only the shape of the type matters, not whether is is generic or
   not. [generic_level] might be somewhat slower, but it ensures
   invariants on types are enforced (decreasing levels.), and we don't
   care about efficiency here.
*)
let apply env params body args =
  try
    subst env generic_level (ref Mnil) None params args body
  with
    Unify _ -> raise Cannot_apply
(*e: function Ctype.apply *)


                              (****************************)
                              (*  Abbreviation expansion  *)
                              (****************************)


(*s: function Ctype.find_expans *)
(* Search whether the expansion has been memorized. *)
let rec find_expans p1 =
  function
    Mnil ->
      None
  | Mcons (p2, ty, _) when Path.same p1 p2 ->
      Some ty
  | Mcons (_, _, rem) ->
      find_expans p1 rem
  | Mlink {contents = rem} ->
      find_expans p1 rem
(*e: function Ctype.find_expans *)

(*s: constant Ctype.previous_env *)
let previous_env = ref Env.empty
(*e: constant Ctype.previous_env *)

(* Expand an abbreviation. The expansion is memorized. *)
(* 
   Assume the level is greater than the path binding time of the
   expanded abbreviation.
*)
(*s: function Ctype.expand_abbrev *)
(*
   An abbreviation expansion will fail in either of these cases:
   1. The type constructor does not correspond to a manifest type.
   2. The type constructor is defined in an external file, and this
      file is not in the path (missing -I options).
   3. The type constructor is not in the "local" environment. This can
      happens when a non-generic type variable has been instantiated
      afterwards to the not yet defined type constructor. (Actually,
      this cannot happen at the moment due to the strong constraints
      between type levels and constructor binding time.)
   4. The expansion requires the expansion of another abbreviation,
      and this other expansion fails.
*)
let expand_abbrev env path args abbrev level =
  (* 
     If the environnement has changed, memorized expansions might not
     be correct anymore, and so we flush the cache. This is safe but
     quite pessimistic: it would be enough to flush the cache at the
     ends of structures and signatures.
     +++ Do it !
  *)
  if env != !previous_env then begin
    cleanup_abbrev ();
    previous_env := env
  end;
  match find_expans path !abbrev with
    Some ty ->
      if level <> generic_level then
        update_level env level ty;
      ty
  | None ->
      let (params, body) =
        try Env.find_type_expansion path env with Not_found ->
          raise Cannot_expand
      in
      try
        subst env level abbrev (Some path) params args body
      with Unify _ -> raise Cannot_expand
(*e: function Ctype.expand_abbrev *)

(*s: function Ctype.try_expand_head *)
(* Fully expand the head of a type. Raise an exception if the type
   cannot be expanded. *)
let rec try_expand_head env ty =
  let ty = repr ty in
  match ty.desc with
    Tconstr(p, tl, abbrev) ->
      let ty' = expand_abbrev env p tl abbrev ty.level in
      begin try
        try_expand_head env ty'
      with Cannot_expand ->
        repr ty'
      end
  | _ ->
      raise Cannot_expand
(*e: function Ctype.try_expand_head *)

(*s: toplevel Ctype._1 *)
let _ = try_expand_head' := try_expand_head
(*e: toplevel Ctype._1 *)

(*s: function Ctype.expand_head *)
(* Fully expand the head of a type. *)
let rec expand_head env ty =
  try try_expand_head env ty with Cannot_expand -> repr ty
(*e: function Ctype.expand_head *)

(*s: function Ctype.full_expand *)
(* Recursively expand the head of a type.
   Also expand #-types. *)
let rec full_expand env ty =
  let ty = repr (expand_head env ty) in
  match ty.desc with
  | _ ->
      ty
(*e: function Ctype.full_expand *)

(*s: function Ctype.generic_abbrev *)
(*
   Check whether the abbreviation expands to a well-defined type.
   During the typing of a class, abbreviations for correspondings
   types expand to non-generic types.
*)
let generic_abbrev env path =
  try
    let (_, body) = Env.find_type_expansion path env in
    (repr body).level = generic_level
  with
    Not_found ->
      false
(*e: function Ctype.generic_abbrev *)


                              (*****************)
                              (*  Occur check  *)
                              (*****************)


(*s: exception Ctype.Occur *)
exception Occur
(*e: exception Ctype.Occur *)

(*s: constant Ctype.visited *)
(* The marks are already used by [expand_abbrev]... *)
let visited = ref []
(*e: constant Ctype.visited *)

(*s: function Ctype.non_recursive_abbrev *)
let rec non_recursive_abbrev env ty =
  let ty = repr ty in
  if ty == none then raise Recursive_abbrev;
  if not (List.memq ty !visited) then begin
    let level = ty.level in
    visited := ty :: !visited;
    match ty.desc with
      Tconstr(p, args, abbrev) ->
        begin try
          non_recursive_abbrev env (try_expand_head env ty)
        with Cannot_expand ->
          iter_type_expr (non_recursive_abbrev env) ty
        end
    | _ ->
        iter_type_expr (non_recursive_abbrev env) ty
  end
(*e: function Ctype.non_recursive_abbrev *)

(*s: function Ctype.correct_abbrev *)
let correct_abbrev env ident params ty =
  visited := [];
  non_recursive_abbrev env
    (subst env generic_level (ref (Mcons (Path.Pident ident, none, Mnil))) None
       [] [] ty);
  visited := []
(*e: function Ctype.correct_abbrev *)

(*s: function Ctype.occur_rec *)
let rec occur_rec env visited ty0 ty =
  if ty == ty0  then raise Occur;
  match ty.desc with
    Tconstr(p, tl, abbrev) ->
      begin try
        if List.memq ty visited then raise Occur;
        iter_type_expr (occur_rec env (ty::visited) ty0) ty
      with Occur -> try
        occur_rec env visited ty0 (try_expand_head env ty)
      with Cannot_expand ->
        raise Occur
      end
  | _ ->
      iter_type_expr (occur_rec env visited ty0) ty
(*e: function Ctype.occur_rec *)

(*s: function Ctype.occur *)
let occur env ty0 ty =
  try occur_rec env [] ty0 ty with Occur -> raise (Unify [])
(*e: function Ctype.occur *)


                              (*****************)
                              (*  Unification  *)
                              (*****************)



(**** Transform error trace ****)
(*s: function Ctype.expand_trace *)
(* +++ Move it to some other place ? *)

let expand_trace env trace =
  List.fold_right
    (fun (t1, t2) rem ->
       (repr t1, full_expand env t1)::(repr t2, full_expand env t2)::rem)
    trace []
(*e: function Ctype.expand_trace *)

(*s: constant Ctype.filter_trace *)
let rec filter_trace =
  function
    (t1, t1')::(t2, t2')::rem ->
      let rem' = filter_trace rem in
      if (t1 == t1') & (t2 == t2')
      then rem'
      else (t1, t1')::(t2, t2')::rem'
  | _ ->
      []
(*e: constant Ctype.filter_trace *)

(**** Unification ****)

(*s: function Ctype.deep_occur *)
(* Return whether [t0] occurs in [ty]. Objects are also traversed. *)
let deep_occur t0 ty =
  let rec occur_rec ty =
    let ty = repr ty in
    if ty.level >= lowest_level then begin
      if ty == t0 then raise Occur;
      ty.level <- pivot_level - ty.level;
      iter_type_expr occur_rec ty
    end
  in
  try
    occur_rec ty; unmark_type ty; false
  with Occur ->
    unmark_type ty; true
(*e: function Ctype.deep_occur *)

(*
   1. When unifying two non-abbreviated types, one type is made a link
      to the other. When unifying an abbreviated type with a
      non-abbreviated type, the non-abbreviated type is made a link to
      the other one. When unifying to abbreviated types, these two
      types are kept distincts, but they are made to (temporally)
      expand to the same type.
   2. Abbreviations with at least one parameter are systematically
      expanded. The overhead does not seem to high, and that way
      abbreviations where some parameters does not appear in the
      expansion, such as ['a t = int], are correctly handled. In
      particular, for this example, unifying ['a t] with ['b t] keeps
      ['a] and ['b] distincts. (Is it really important ?)
   3. Unifying an abbreviation ['a t = 'a] with ['a] should not yield
      ['a t as 'a]. Indeed, the type variable would otherwise be lost.
      This problem occurs for abbreviations expanding to a type
      variable, but also to many other constrained abbreviations (for
      instance, [(< x : 'a > -> unit) t = <x : 'a>]). The solution is
      that, if an abbreviation is unified with some subpart of its
      parameters, then the parameter actually does not get
      abbreviated.  It would be possible to check whether some
      information is indeed lost, but it probably does not worth it.
*)
let rec unify env t1 t2 =
  (* First step: special cases (optimizations) *)
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else

  try
    match (t1.desc, t2.desc) with
      (Tvar, Tconstr _) when deep_occur t1 t2 ->
        unify2 env t1 t2
    | (Tconstr _, Tvar) when deep_occur t2 t1 ->
        unify2 env t1 t2
    | (Tvar, _) ->
        occur env t1 t2;
        update_level env t1.level t2;
        t1.desc <- Tlink t2
    | (_, Tvar) ->
        occur env t2 t1;
        update_level env t2.level t1;
        t2.desc <- Tlink t1
    | (Tconstr (p1, [], _), Tconstr (p2, [], _)) when Path.same p1 p2 ->
        update_level env t1.level t2;
        t1.desc <- Tlink t2
    | _ ->
        unify2 env t1 t2
  with Unify trace ->
    raise (Unify ((t1, t2)::trace))

and unify2 env t1 t2 =
  (* Second step: expansion of abbreviations *)
  let t1' = expand_head env t1 in
  let t2' = expand_head env t2 in
  (* Expansion may have changed the representative of the types... *)
  let t1' = repr t1' and t2' = repr t2' in
  if t1' == t2' then () else

  let t1 = repr t1 and t2 = repr t2 in
  if (t1 == t1') || (t2 != t2') then
    unify3 env t1 t1' t2 t2'
  else
    try unify3 env t2 t2' t1 t1' with Unify trace ->
      raise (Unify (List.map (fun (x, y) -> (y, x)) trace))

and unify3 env t1 t1' t2 t2' =
  (* Third step: truly unification *)
  (* Assumes either [t1 == t1'] or [t2 != t2'] *)
  let d1 = t1'.desc and d2 = t2'.desc in
  
  if (t2 != t2') && (deep_occur t1' t2) then begin
    (* See point 3. *)
    occur env t1' t2';
    update_level env t1'.level t2';
    t1'.desc <- Tlink t2'
  end else begin
    occur env t1' t2;
    update_level env t1'.level t2;
    t1'.desc <- Tlink t2
  end;

  try
    begin match (d1, d2) with
      (Tvar, _) ->
        ()
    | (_, Tvar) ->
        occur env t2' (newty d1);
        if t1 == t1' then begin
          (* The variable must be instantiated... *)
          let ty = {desc = d1; level = t1'.level} in
          update_level env t2'.level ty;
          t2'.desc <- Tlink ty
        end else begin
          t1'.desc <- d1;
          update_level env t2'.level t1;
          t2'.desc <- Tlink t1
        end
    | (Tarrow (t1, u1), Tarrow (t2, u2)) ->
        unify env t1 t2; unify env u1 u2
    | (Ttuple tl1, Ttuple tl2) ->
        unify_list env tl1 tl2
    | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _)) when Path.same p1 p2 ->
        unify_list env tl1 tl2
    | (Tnil, Tnil) ->
        ()
    | (_, _) ->
        raise (Unify [])
    end
(*
    (* 
       Can only be done afterwards, once the row variable has
       (possibly) been instantiated.
    *)
    if t1 != t1' (* && t2 != t2' *) then begin
      match (t1.desc, t2.desc) with
        (Tconstr (p, ty::_, _), _)
            when ((repr ty).desc <> Tvar)
              && weak_abbrev p
              && not (deep_occur t1 t2) ->
          update_level env t1.level t2;
          t1.desc <- Tlink t2
      | (_, Tconstr (p, ty::_, _))
            when ((repr ty).desc <> Tvar)
              && weak_abbrev p
              && not (deep_occur t2 t1) ->
          update_level env t2.level t1;
          t2.desc <- Tlink t1;
          t1'.desc <- Tlink t2'
      | _ ->
          ()
    end
*)
  with Unify trace ->
    t1'.desc <- d1;
    raise (Unify trace)

and unify_list env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (unify env) tl1 tl2



(*s: function Ctype.unify *)
let unify env ty1 ty2 =
  try
    unify env ty1 ty2
  with Unify trace ->
    let trace = expand_trace env trace in
    match trace with
      t1::t2::rem ->
        raise (Unify (t1::t2::filter_trace rem))
    | _ ->
        fatal_error "Ctype.unify"
(*e: function Ctype.unify *)

(*s: toplevel Ctype._2 *)
let _ = unify' := unify
(*e: toplevel Ctype._2 *)

(**** Special cases of unification ****)

(*s: function Ctype.filter_arrow *)
(* Unify [t] and ['a -> 'b]. Return ['a] and ['b]. *)
let rec filter_arrow env t =
  let t = expand_head env t in
  match t.desc with
    Tvar ->
      let t1 = newvar () and t2 = newvar () in
      let t' = newty (Tarrow (t1, t2)) in
      update_level env t.level t';
      t.desc <- Tlink t';
      (t1, t2)
  | Tarrow(t1, t2) ->
      (t1, t2)
  | _ ->
      raise (Unify [])
(*e: function Ctype.filter_arrow *)



                        (***********************************)
                        (*  Matching between type schemes  *)
                        (***********************************)

(*s: function Ctype.moregen_occur *)
(*
   Update the level of [ty]. First check that the levels of variables
   from the subject are not lowered.
*)
let moregen_occur env level ty = 
  let rec occur ty =
    let ty = repr ty in
    if ty.level > level then begin
      if ty.desc = Tvar then raise Occur;
      ty.level <- pivot_level - ty.level;
      iter_type_expr occur ty
    end
  in
  begin try
    occur ty; unmark_type ty
  with Occur ->
    unmark_type ty; raise (Unify [])
  end;
  update_level env level ty
(*e: function Ctype.moregen_occur *)

(*s: function Ctype.moregeneral *)
(*
   Non-generic variable can be instanciated only if [inst_nongen] is
   true. So, [inst_nongen] should be set to false if the subject might
   contain non-generic variables.
   Usually, the subject is given by the user, and the pattern
   is unimportant.  So, no need to propagate abbreviations.
*)
let moregeneral env inst_nongen pat_sch subj_sch =
  let type_pairs = ref [] in

  let rec moregen env t1 t2 =
    if t1 == t2 then () else
    let t1 = repr t1 in
    let t2 = repr t2 in
    if t1 == t2 then () else

    match (t1.desc, t2.desc) with
      (Tvar, _) when if inst_nongen then t1.level <> generic_level - 1
                                    else t1.level =  generic_level ->
        moregen_occur env t1.level t2;
        t1.desc <- Tlink t2
    | (Tconstr (p1, [], _), Tconstr (p2, [], _)) when Path.same p1 p2 ->
        ()
    | _ ->
        let t1' = expand_head env t1 in
        let t2' = expand_head env t2 in
        (* Expansion may have changed the representative of the types... *)
        let t1' = repr t1' and t2' = repr t2' in
        if t1' == t2' then () else
        if
          List.exists (function (t1, t2) -> t1 == t1' && t2 == t2') !type_pairs
        then
          ()
        else begin
          type_pairs := (t1', t2') :: !type_pairs;

          match (t1'.desc, t2'.desc) with
            (Tvar, _) when if inst_nongen then t1'.level <> generic_level - 1
                                          else t1'.level =  generic_level ->
              moregen_occur env t1'.level t2;
              t1'.desc <- Tlink t2
          | (Tarrow (t1, u1), Tarrow (t2, u2)) ->
              moregen env t1 t2; moregen env u1 u2
          | (Ttuple tl1, Ttuple tl2) ->
              moregen_list env tl1 tl2
          | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _))
                when Path.same p1 p2 ->
              moregen_list env tl1 tl2
          | (Tnil, Tnil) ->
              ()
          | (_, _) ->
              raise (Unify [])
        end

  and moregen_list env tl1 tl2 =
    if List.length tl1 <> List.length tl2 then
      raise (Unify []);
    List.iter2 (moregen env) tl1 tl2

  in
  let old_level = !current_level in
  current_level := generic_level - 1;
  (*
     Generic variables are first duplicated with [instance].  So,
     their levels are lowered to [generic_level - 1].  The subject is
     then copied with [correct_levels].  That way, its levels won't be
     changed.
  *)
  let subj = correct_levels (instance subj_sch) in
  current_level := generic_level;
  (* Duplicate generic variables *)
  let patt = instance pat_sch in
  let res = try moregen env patt subj; true with Unify _ -> false in
  current_level := old_level;
  res
(*e: function Ctype.moregeneral *)


                 (*********************************************)
                 (*  Equivalence between parameterized types  *)
                 (*********************************************)


(*s: function Ctype.equal *)
(* Two modes: with or without renaming of variables *)

let equal env rename tyl1 tyl2 =
  let subst = ref [] in
  let type_pairs = ref [] in

  let rec eqtype t1 t2 =
    if t1 == t2 then true else
    let t1 = repr t1 in
    let t2 = repr t2 in
    if t1 == t2 then true else
    match (t1.desc, t2.desc) with
    | (Tvar, Tvar) when rename ->
        begin try
          List.assq t1 !subst == t2
        with Not_found ->
          subst := (t1, t2) :: !subst;
          true
        end
    | (Tconstr (p1, [], _), Tconstr (p2, [], _)) when Path.same p1 p2 ->
        true
    | _ ->
        let t1 = expand_head env t1 in
        let t2 = expand_head env t2 in
        (* Expansion may have changed the representative of the types... *)
        let t1 = repr t1 and t2 = repr t2 in
        if t1 == t2 then true else
        List.exists (function (t1', t2') -> t1 == t1' & t2 == t2') !type_pairs
              (* XXX Possibly slow... *)
          ||
        begin
          type_pairs := (t1, t2) :: !type_pairs;
          match (t1.desc, t2.desc) with
            (Tvar, Tvar) when rename ->
              begin try
                List.assq t1 !subst == t2
              with Not_found ->
                subst := (t1, t2) :: !subst;
                true
              end
          | (Tarrow (t1, u1), Tarrow (t2, u2)) ->
              eqtype t1 t2 && eqtype u1 u2
          | (Ttuple tl1, Ttuple tl2) ->
              eqtype_list tl1 tl2
          | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _))
                when Path.same p1 p2 ->
              eqtype_list tl1 tl2
          | (Tnil, Tnil) ->
              true
          | (_, _) ->
              false
        end

  and eqtype_list tl1 tl2 =
    List.length tl1 = List.length tl2
                   &&
    List.for_all2 eqtype tl1 tl2

  in
    eqtype_list tyl1 tyl2
(*e: function Ctype.equal *)


                              (***************)
                              (*  Subtyping  *)
                              (***************)


(*s: constant Ctype.subtypes *)
(**** Build a subtype of a given type. ****)

let subtypes = ref []
(*e: constant Ctype.subtypes *)

(*s: function Ctype.build_subtype *)
(* XXX Types r�cursifs ? *)
let rec build_subtype env t =
  let t = repr t in
  match t.desc with
    Tlink t' ->                         (* Redundant ! *)
      build_subtype env t'
  | Tvar ->
      (t, false)
  | Tarrow(t1, t2) ->
      let (t1', c1) = (t1, false) in
      let (t2', c2) = build_subtype env t2 in
      if c1 or c2 then (new_global_ty (Tarrow(t1', t2')), true)
      else (t, false)
  | Ttuple tlist ->
      let (tlist', clist) =
        List.split (List.map (build_subtype env) tlist)
      in
      if List.exists (function c -> c) clist then
        (new_global_ty (Ttuple tlist'), true)
      else (t, false)
  | Tconstr(p, tl, abbrev) when generic_abbrev env p ->
      let t' = expand_abbrev env p tl abbrev t.level in
      let (t'', c) = build_subtype env t' in
      if c then (t'', true)
      else (t, false)
  | Tconstr(p, tl, abbrev) ->
      (t, false)
  | Tnil ->
      let v = new_global_var () in
      (v, true)
(*e: function Ctype.build_subtype *)

(*s: function Ctype.enlarge_type *)
let enlarge_type env ty =
  subtypes := [];
  let (ty', _) = build_subtype env ty in
  subtypes := [];
  ty'
(*e: function Ctype.enlarge_type *)

(**** Check whether a type is a subtype of another type. ****)

(*s: constant Ctype.subtypes (./typing/ctype.ml) *)
(*
    During the traversal, a trace of visited types is maintained. It
    is printed in case of error.
    Constraints (pairs of types that must be equals) are accumulated
    rather than being enforced straight. Indeed, the result would
    otherwise depend on the order in which these constraints are
    enforced.
    A function enforcing these constraints is returned. That way, type
    variables can be bound to their actual values before this function
    is called (see Typecore).
    Only well-defined abbreviations are expanded (hence the tests
    [generic_abbrev ...]).
*)

let subtypes = ref [];;
(*e: constant Ctype.subtypes (./typing/ctype.ml) *)

(*s: function Ctype.subtype_error *)
let subtype_error env trace =
  raise (Subtype (expand_trace env (List.rev trace), []))
(*e: function Ctype.subtype_error *)

let rec subtype_rec env trace t1 t2 =
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then [] else
  if List.exists (fun (t1', t2') -> t1 == t1' & t2 == t2') !subtypes then
      (* +++ Possibly slow *)
    []
  else begin
    subtypes := (t1, t2) :: !subtypes;
    match (t1.desc, t2.desc) with
      (Tvar, _) | (_, Tvar) ->
        [(trace, t1, t2)]
    | (Tarrow(t1, u1), Tarrow(t2, u2)) ->
        (subtype_rec env ((t2, t1)::trace) t2 t1) @
        (subtype_rec env ((u1, u2)::trace) u1 u2)
    | (Ttuple tl1, Ttuple tl2) ->
        subtype_list env trace tl1 tl2
    | (Tconstr(p1, tl1, abbrev1), Tconstr _) when generic_abbrev env p1 ->
        subtype_rec env trace (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
    | (Tconstr _, Tconstr(p2, tl2, abbrev2)) when generic_abbrev env p2 ->
        subtype_rec env trace t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
    | (Tconstr _, Tconstr _) ->
        [(trace, t1, t2)]
    | (Tconstr(p1, tl1, abbrev1), _) when generic_abbrev env p1 ->
        subtype_rec env trace (expand_abbrev env p1 tl1 abbrev1 t1.level) t2
    | (_, Tconstr (p2, tl2, abbrev2)) when generic_abbrev env p2 ->
        subtype_rec env trace t1 (expand_abbrev env p2 tl2 abbrev2 t2.level)
    | (_, _) ->
        subtype_error env trace
  end

and subtype_list env trace tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    subtype_error env trace;
  List.fold_left2
    (fun cstrs t1 t2 -> cstrs @ (subtype_rec env ((t1, t2)::trace) t1 t2))
    [] tl1 tl2


(*s: function Ctype.subtype *)
let subtype env ty1 ty2 =
  subtypes := [];
  (* Build constraint set. *)
  let cstrs = subtype_rec env [(ty1, ty2)] ty1 ty2 in
  (* Enforce constraints. *)
  function () ->
    List.iter
      (function (trace0, t1, t2) ->
         try unify env t1 t2 with Unify trace ->
           raise (Subtype (expand_trace env (List.rev trace0),
                           List.tl (List.tl trace))))
      cstrs;
    subtypes := []
(*e: function Ctype.subtype *)


                              (*******************)
                              (*  Miscellaneous  *)
                              (*******************)


(*s: function Ctype.unalias *)
let unalias ty =
  let ty = repr ty in
  match ty.desc with
    Tvar ->
      ty
  | _ ->
      {desc = ty.desc; level = ty.level}
(*e: function Ctype.unalias *)

(*s: function Ctype.unroll_abbrev *)
let unroll_abbrev id tl ty =
  let ty = repr ty in
  if (ty.desc = Tvar) || (List.exists (deep_occur ty) tl) then
    ty
  else
    let ty' = {desc = ty.desc; level = ty.level} in
    ty.desc <- Tlink {desc = Tconstr (Path.Pident id, tl, ref Mnil);
                      level = ty.level};
    ty'
(*e: function Ctype.unroll_abbrev *)

(*s: function Ctype.arity *)
(* Return the arity (as for curried functions) of the given type. *)
let rec arity ty =
  match (repr ty).desc with
    Tarrow(t1, t2) -> 1 + arity t2
  | _ -> 0
(*e: function Ctype.arity *)

(*s: function Ctype.cyclic_abbrev *)
(* Check whether an abbreviation expands to itself. *)
let rec cyclic_abbrev env id ty =
  let ty = repr ty in
  match ty.desc with
    Tconstr (Path.Pident id', _, _) when Ident.same id id' ->
      true
  | Tconstr (p, tl, abbrev) ->
      begin try
        cyclic_abbrev env id (try_expand_head env ty)
      with Cannot_expand ->
        false
      end
  | _ ->
      false
(*e: function Ctype.cyclic_abbrev *)


                              (*************************)
                              (*  Remove dependencies  *)
                              (*************************)


(*s: function Ctype.nondep_type_rec *)
(*
   Variables are left unchanged. Other type nodes are duplicated, with
   levels set to generic level.
   During copying, the description of a (non-variable) node is first
   replaced by a link to a marked stub ([Tlink (newmarkedgenvar ())]).
   The mark allows to differentiate the original type from the copy.
   Once the copy is made, it replaces the stub.
   After copying, the description of node, which was stored by
   [save_desc], must be put back, using [cleanup_types], and the
   marks on the copy must be removed.
*)

let rec nondep_type_rec env id ty =
  let ty = repr ty in
  if (ty.desc = Tvar) || (ty.level < lowest_level) then
    ty
  else begin
    let desc = ty.desc in
    save_desc ty desc;
    let ty' = newmarkedgenvar () in        (* Stub *)
    ty.desc <- Tlink ty';
    ty'.desc <-
      begin match desc with
        Tvar ->
          fatal_error "Ctype.nondep_type_rec"
      | Tarrow(t1, t2) ->
          Tarrow(nondep_type_rec env id t1, nondep_type_rec env id t2)
      | Ttuple tl ->
          Ttuple(List.map (nondep_type_rec env id) tl)
      | Tconstr(p, tl, abbrev) ->
          if Path.isfree id p then
            begin try
              Tlink (nondep_type_rec env id
                       (expand_abbrev env p tl abbrev ty.level))
              (*
                 The [Tlink] is important. The expanded type may be a
                 variable, or may not be completely copied yet
                 (recursive type), so one cannot just take its
                 description.
               *)
            with Cannot_expand ->
              raise Not_found
            end
          else
            Tconstr(p, List.map (nondep_type_rec env id) tl, ref Mnil)
      | Tnil ->
          Tnil
      | Tlink ty ->                    (* Actually unused *)
          Tlink(nondep_type_rec env id ty)
      end;
    ty'
  end
(*e: function Ctype.nondep_type_rec *)

(*s: function Ctype.nondep_type *)
let nondep_type env id ty =
  try
    let ty' = nondep_type_rec env id ty in
    cleanup_types ();
    unmark_type ty';
    ty'
  with Not_found ->
    cleanup_types ();
    raise Not_found
(*e: function Ctype.nondep_type *)

(*s: function Ctype.nondep_type_decl *)
(* Preserve sharing inside type declarations. *)
let nondep_type_decl env mid id is_covariant decl =
  try
    let params = List.map (nondep_type_rec env mid) decl.type_params in
    let decl =
      { type_params = params;
        type_arity = decl.type_arity;
        type_kind =
          begin try
            match decl.type_kind with
              Type_abstract ->
                Type_abstract
            | Type_variant cstrs ->
                Type_variant(List.map
                  (fun (c, tl) -> (c, List.map (nondep_type_rec env mid) tl))
                  cstrs)
            | Type_record lbls ->
                Type_record(List.map
                  (fun (c, mut, t) -> (c, mut, nondep_type_rec env mid t))
                  lbls)
          with Not_found when is_covariant ->
            Type_abstract
          end;
        type_manifest =
          begin try
            match decl.type_manifest with
              None -> None
            | Some ty ->
                Some (unroll_abbrev id params (nondep_type_rec env mid ty))
          with Not_found when is_covariant ->
            None
          end }
    in
    cleanup_types ();
    List.iter unmark_type decl.type_params;
    begin match decl.type_kind with
      Type_abstract -> ()
    | Type_variant cstrs ->
        List.iter (fun (c, tl) -> List.iter unmark_type tl) cstrs
    | Type_record lbls ->
        List.iter (fun (c, mut, t) -> unmark_type t) lbls
    end;
    begin match decl.type_manifest with
      None    -> ()
    | Some ty -> unmark_type ty
    end;
    decl
  with Not_found ->
    cleanup_types ();
    raise Not_found
(*e: function Ctype.nondep_type_decl *)


                    (**************************************)
                    (*  Check genericity of type schemes  *)
                    (**************************************)


(*s: type Ctype.closed_schema_result (./typing/ctype.ml) *)
type closed_schema_result = Var of type_expr | Row_var of type_expr
(*e: type Ctype.closed_schema_result (./typing/ctype.ml) *)
(*s: exception Ctype.Failed *)
exception Failed of closed_schema_result
(*e: exception Ctype.Failed *)

(*s: function Ctype.closed_schema_rec *)
let rec closed_schema_rec row ty =
  let ty = repr ty in
  if ty.level >= lowest_level then begin
    let level = ty.level in
    ty.level <- pivot_level - level;
    match ty.desc with
      Tvar when level <> generic_level ->
        raise (Failed (if row then Row_var ty else Var ty))
    | _ ->
        iter_type_expr (closed_schema_rec false) ty
  end
(*e: function Ctype.closed_schema_rec *)

(*s: function Ctype.closed_schema *)
(* Return whether all variables of type [ty] are generic. *)
let closed_schema ty =
  try
    closed_schema_rec false ty;
    unmark_type ty;
    true
  with Failed _ ->
    unmark_type ty;
    false
(*e: function Ctype.closed_schema *)

(*e: ./typing/ctype.ml *)
