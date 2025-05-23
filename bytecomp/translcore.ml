(*s: bytecomp/translcore.ml *)
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

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Misc
open Asttypes
open Primitive
open Path
open Types
open Typedtree
open Lambda

(*s: type [[Translcore.error]] *)
type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr
(*e: type [[Translcore.error]] *)

(*s: exception [[Translcore.Error]] *)
exception Error of Location.t * error
(*e: exception [[Translcore.Error]] *)

(*s: constant [[Translcore.comparisons_table]] *)
(* Translation of primitives *)

let comparisons_table = create_hashtable 11 [
  "%equal",
      (Pccall{prim_name = "equal"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Ceq,
       Pfloatcomp Ceq,
       Pccall{prim_name = "string_equal"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false});
  "%notequal",
      (Pccall{prim_name = "notequal"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Cneq,
       Pfloatcomp Cneq,
       Pccall{prim_name = "string_notequal"; prim_arity = 2;
              prim_alloc = false; prim_native_name = ""; 
              prim_native_float = false});
  "%lessthan",
      (Pccall{prim_name = "lessthan"; prim_arity = 2; prim_alloc = false; 
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Clt,
       Pfloatcomp Clt,
       Pccall{prim_name = "lessthan"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false});
  "%greaterthan",
      (Pccall{prim_name = "greaterthan"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Cgt,
       Pfloatcomp Cgt,
       Pccall{prim_name = "greaterthan"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false});
  "%lessequal",
      (Pccall{prim_name = "lessequal"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Cle,
       Pfloatcomp Cle,
       Pccall{prim_name = "lessequal"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false});
  "%greaterequal",
      (Pccall{prim_name = "greaterequal"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Cge,
       Pfloatcomp Cge,
       Pccall{prim_name = "greaterequal"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false})
]
(*e: constant [[Translcore.comparisons_table]] *)

(*s: constant [[Translcore.primitives_table]] *)
let primitives_table = create_hashtable 31 [
  "%identity", Pidentity;

  "%field0", Pfield 0;
  "%field1", Pfield 1;
  "%setfield0", Psetfield(0, true);
  "%makeblock", Pmakeblock(0, Immutable);
  "%makemutable", Pmakeblock(0, Mutable);

  "%raise", Praise;

  "%sequand", Psequand;
  "%sequor", Psequor;
  "%boolnot", Pnot;

  "%negint", Pnegint;
  "%succint", Poffsetint 1;
  "%predint", Poffsetint(-1);
  "%addint", Paddint;
  "%subint", Psubint;
  "%mulint", Pmulint;
  "%divint", Pdivint;
  "%modint", Pmodint;
  "%andint", Pandint;
  "%orint", Porint;
  "%xorint", Pxorint;
  "%lslint", Plslint;
  "%lsrint", Plsrint;
  "%asrint", Pasrint;

  "%eq", Pintcomp Ceq;
  "%noteq", Pintcomp Cneq;
  "%ltint", Pintcomp Clt;
  "%leint", Pintcomp Cle;
  "%gtint", Pintcomp Cgt;
  "%geint", Pintcomp Cge;

  "%incr", Poffsetref(1);
  "%decr", Poffsetref(-1);

  "%intoffloat", Pintoffloat;
  "%floatofint", Pfloatofint;
  "%negfloat", Pnegfloat;
  "%absfloat", Pabsfloat;
  "%addfloat", Paddfloat;
  "%subfloat", Psubfloat;
  "%mulfloat", Pmulfloat;
  "%divfloat", Pdivfloat;
  "%eqfloat", Pfloatcomp Ceq;
  "%noteqfloat", Pfloatcomp Cneq;
  "%ltfloat", Pfloatcomp Clt;
  "%lefloat", Pfloatcomp Cle;
  "%gtfloat", Pfloatcomp Cgt;
  "%gefloat", Pfloatcomp Cge;

  "%string_length", Pstringlength;
  "%string_safe_get", Pstringrefs;
  "%string_safe_set", Pstringsets;
  "%string_unsafe_get", Pstringrefu;
  "%string_unsafe_set", Pstringsetu;

  "%array_length", Parraylength Pgenarray;
  "%array_safe_get", Parrayrefs Pgenarray;
  "%array_safe_set", Parraysets Pgenarray;
  "%array_unsafe_get", Parrayrefu Pgenarray;
  "%array_unsafe_set", Parraysetu Pgenarray;
  "%obj_size", Parraylength Paddrarray;
  "%obj_field", Parrayrefu Paddrarray;
  "%obj_set_field", Parraysetu Paddrarray;
  "%obj_is_int", Pisint;
]
(*e: constant [[Translcore.primitives_table]] *)

let same_base_type ty1 ty2 =
  match (Ctype.repr ty1, Ctype.repr ty2) with
    (Tconstr(p1, []), Tconstr(p2, [])) -> Path.same p1 p2
  | (_, _) -> false

let maybe_pointer arg =
  not(same_base_type arg.exp_type Predef.type_int or
      same_base_type arg.exp_type Predef.type_char)


let array_kind arg =
  match Ctype.repr arg.exp_type with
    Tconstr(p, [ty]) when Path.same p Predef.path_array ->
      begin match Ctype.repr ty with
        Tvar v -> Pgenarray
      | Tconstr(p, _) ->
          if Path.same p Predef.path_int or Path.same p Predef.path_char then
            Pintarray
          else if Path.same p Predef.path_float then
            Pfloatarray
          else
            Paddrarray
      | _ -> Paddrarray
      end
  | _ -> Pgenarray (* This can happen with abbreviations that we can't expand
                      here because the typing environment is lost *)

let has_base_type exp base_ty = 
  same_base_type exp.exp_type base_ty

(*s: function [[Translcore.has_base_type]] *)
(*
let has_base_type exp base_ty =
  let exp_ty =
    Ctype.expand_head exp.exp_env (Ctype.correct_levels exp.exp_type) in
  match (Ctype.repr exp_ty, Ctype.repr base_ty) with
    {desc = Tconstr(p1, _, _)}, {desc = Tconstr(p2, _, _)} -> Path.same p1 p2
  | (_, _) -> false
*)
(*e: function [[Translcore.has_base_type]] *)

(*s: function [[Translcore.maybe_pointer]] *)
(*
let maybe_pointer arg =
  not(has_base_type arg Predef.type_int or has_base_type arg Predef.type_char)
*)
(*e: function [[Translcore.maybe_pointer]] *)

(*s: function [[Translcore.array_element_kind]] *)
(*
let array_element_kind env ty =
  let ty = Ctype.repr (Ctype.expand_head env ty) in
  match ty.desc with
    Tvar ->
      Pgenarray
  | Tconstr(p, args, abbrev) ->
      if Path.same p Predef.path_int || Path.same p Predef.path_char then
        Pintarray
      else if Path.same p Predef.path_float then
        Pfloatarray
      else if Path.same p Predef.path_string
           || Path.same p Predef.path_array then
        Paddrarray
      else begin
        try
          match Env.find_type p env with
            {type_kind = Type_abstract} ->
              Pgenarray
          | {type_kind = Type_variant cstrs}
            when List.for_all (fun (name, args) -> args = []) cstrs ->
              Pintarray
          | {type_kind = _} ->
              Paddrarray
        with Not_found ->
          (* This can happen due to e.g. missing -I options,
             causing some .cmi files to be unavailable.
             Maybe we should emit a warning. *)
          Pgenarray
      end
  | _ ->
      Paddrarray
*)
(*e: function [[Translcore.array_element_kind]] *)

(*s: function [[Translcore.array_kind]] *)
(*
let array_kind arg =
  let ty = Ctype.correct_levels arg.exp_type in
  let array_ty = Ctype.expand_head arg.exp_env ty in
  match (Ctype.repr array_ty).desc with
    Tconstr(p, [elt_ty], _) when Path.same p Predef.path_array ->
      array_element_kind arg.exp_env elt_ty
  | _ ->
    fatal_error "Translcore.array_kind"
*)
(*e: function [[Translcore.array_kind]] *)

(*s: constant [[Translcore.prim_makearray]] *)
let prim_makearray =
  { prim_name = "make_vect"; prim_arity = 2; prim_alloc = true;
    prim_native_name = ""; prim_native_float = false }
(*e: constant [[Translcore.prim_makearray]] *)

(*s: function [[Translcore.transl_prim]] *)
let transl_prim prim args =
  try
    let (gencomp, intcomp, floatcomp, stringcomp) =
      Hashtbl.find comparisons_table prim.prim_name in
    begin match args with
      [arg1; {exp_desc = Texp_construct({cstr_tag = Cstr_constant _}, _)}] ->
        intcomp
    | [{exp_desc = Texp_construct({cstr_tag = Cstr_constant _}, _)}; arg2] ->
        intcomp
    | [arg1; arg2] when has_base_type arg1 Predef.type_int
                     or has_base_type arg1 Predef.type_char ->
        intcomp
    | [arg1; arg2] when has_base_type arg1 Predef.type_float ->
        floatcomp
    | [arg1; arg2] when has_base_type arg1 Predef.type_string ->
        stringcomp
    | _ ->
        gencomp
    end
  with Not_found ->
  try
    let p = Hashtbl.find primitives_table prim.prim_name in
    (* Try strength reduction based on the type of the argument *)
    begin match (p, args) with
        (Psetfield(n, _), [arg1; arg2]) -> Psetfield(n, maybe_pointer arg2)
      | (Parraylength Pgenarray, [arg])   -> Parraylength(array_kind arg)
      | (Parrayrefu Pgenarray, arg1 :: _) -> Parrayrefu(array_kind arg1)
      | (Parraysetu Pgenarray, arg1 :: _) -> Parraysetu(array_kind arg1)
      | (Parrayrefs Pgenarray, arg1 :: _) -> Parrayrefs(array_kind arg1)
      | (Parraysets Pgenarray, arg1 :: _) -> Parraysets(array_kind arg1)
      | _ -> p
    end
  with Not_found ->
    Pccall prim
(*e: function [[Translcore.transl_prim]] *)

(*s: function [[Translcore.transl_primitive]] *)
(* Eta-expand a primitive without knowing the types of its arguments *)

let transl_primitive p =
  let prim =
    try
      let (gencomp, intcomp, floatcomp, stringcomp) =
        Hashtbl.find comparisons_table p.prim_name in
      gencomp
    with Not_found ->
    try
      Hashtbl.find primitives_table p.prim_name
    with Not_found ->
      Pccall p in
  let rec make_params n =
    if n <= 0 then [] else Ident.create "prim" :: make_params (n-1) in
  let params = make_params p.prim_arity in
  Lfunction(Curried, params, Lprim(prim, List.map (fun id -> Lvar id) params))
(*e: function [[Translcore.transl_primitive]] *)

(* To check the well-formedness of r.h.s. of "let rec" definitions *)

module IdentSet = Set

(*s: function [[Translcore.check_recursive_lambda]] *)
let check_recursive_lambda idlist lam =
  let rec check_top = function
      Lfunction(kind, params, body) as funct -> true
    | Lprim(Pmakeblock(tag, mut), args) -> List.for_all check args
    | Lprim(Pmakearray kind, args) -> List.for_all check args
    | Llet(str, id, arg, body) -> check arg & check_top body
    | Lletrec(bindings, body) ->
        List.for_all (fun (id, arg) -> check arg) bindings & check_top body
    | Levent (lam, _) -> check_top lam
    | _ -> false
  and check = function
      Lvar _ -> true
    | Lconst cst -> true
    | Lfunction(kind, params, body) -> true
    | Llet(str, id, arg, body) -> check arg & check body
    | Lletrec(bindings, body) ->
        List.for_all (fun (id, arg) -> check arg) bindings & check body
    | Lprim(Pmakeblock(tag, mut), args) -> List.for_all check args
    | Lprim(Pmakearray kind, args) -> List.for_all check args
    | Levent (lam, _) -> check lam
    | lam ->
        let fv = free_variables lam in
        List.for_all (fun id -> not(IdentSet.mem id fv)) idlist
  in check_top lam
(*e: function [[Translcore.check_recursive_lambda]] *)

(*s: exception [[Translcore.Not_constant]] *)
(* To propagate structured constants *)

exception Not_constant
(*e: exception [[Translcore.Not_constant]] *)

(*s: constant [[Translcore.extract_constant]] *)
let extract_constant = function
    Lconst sc -> sc
  | _ -> raise Not_constant
(*e: constant [[Translcore.extract_constant]] *)

(*s: constant [[Translcore.extract_float]] *)
let extract_float = function
    Const_base(Const_float f) -> f
  | _ -> fatal_error "Translcore.extract_float"
(*e: constant [[Translcore.extract_float]] *)

(*s: function [[Translcore.name_pattern]] *)
(* To find reasonable names for let-bound and lambda-bound idents *)

let rec name_pattern default = function
    [] -> Ident.create default
  | (p, e) :: rem ->
      match p.pat_desc with
        Tpat_var id -> id
      | Tpat_alias(p, id) -> id
      | _ -> name_pattern default rem
(*e: function [[Translcore.name_pattern]] *)

(*s: function [[Translcore.event_before]] *)
(* Insertion of debugging events *)

let event_before exp lam =
  if !Clflags.debug && lam <> Lstaticfail
  then Levent(lam, {lev_loc = exp.exp_loc.Location.loc_start;
                    lev_kind = Lev_before;
                    lev_repr = None;
                    lev_env =  ()(* Env.summary exp.exp_env*)})
  else lam
(*e: function [[Translcore.event_before]] *)

(*s: function [[Translcore.event_after]] *)
let event_after exp lam =
  if !Clflags.debug
  then Levent(lam, {lev_loc = exp.exp_loc.Location.loc_end;
                    lev_kind = Lev_after exp.exp_type;
                    lev_repr = None;
                    lev_env = () (* Env.summary exp.exp_env*)})
  else lam
(*e: function [[Translcore.event_after]] *)

(*s: function [[Translcore.event_function]] *)
let event_function exp lam =
  if !Clflags.debug then
    let repr = Some (ref 0) in
    let (info, body) = lam repr in
    (info,
     Levent(body, {lev_loc = exp.exp_loc.Location.loc_start;
                   lev_kind = Lev_function;
                   lev_repr = repr;
                   lev_env = () (* Env.summary  exp.exp_env*)}))
  else
    lam None
(*e: function [[Translcore.event_function]] *)

(* Translation of expressions *)
(*s: function [[Translcore.transl_exp]] *)
let rec transl_exp e =
  match e.exp_desc with
  (*s: [[Translcore.transl_exp()]] cases *)
      Texp_ident(path, {val_prim = Some p}) ->
        transl_primitive p
    | Texp_ident(path, desc) ->
        transl_path path
    | Texp_constant cst ->
        Lconst(Const_base cst)
    | Texp_let(rec_flag, pat_expr_list, body) ->
        transl_let rec_flag pat_expr_list (event_before body (transl_exp body))
    | Texp_function pat_expr_list ->
        let ((kind, params), body) =
          event_function e
            (function repr ->
               transl_function e.exp_loc !Clflags.native_code repr pat_expr_list)
        in
        Lfunction(kind, params, body)
    | Texp_apply({exp_desc = Texp_ident(path, {val_prim = Some p})}, args)
      when List.length args = p.prim_arity ->
        let prim = transl_prim p args in
        let lam = Lprim(prim, transl_list args) in
        begin match prim with Pccall _ -> event_after e lam | _ -> lam end
    | Texp_apply(funct, args) ->
        let lam =
          match transl_exp funct with
           lexp ->
              Lapply(lexp, transl_list args) in
        event_after e lam
    | Texp_match({exp_desc = Texp_tuple argl} as arg, pat_expr_list) ->
        Matching.for_multiple_match e.exp_loc
          (transl_list argl) (transl_cases pat_expr_list)
    | Texp_match(arg, pat_expr_list) ->
        Matching.for_function e.exp_loc None
          (transl_exp arg) (transl_cases pat_expr_list)
    | Texp_try(body, pat_expr_list) ->
        let id = name_pattern "exn" pat_expr_list in
        Ltrywith(transl_exp body, id,
                 Matching.for_trywith (Lvar id) (transl_cases pat_expr_list))
    | Texp_tuple el ->
        let ll = transl_list el in
        begin try
          Lconst(Const_block(0, List.map extract_constant ll))
        with Not_constant ->
          Lprim(Pmakeblock(0, Immutable), ll)
        end
    | Texp_construct(cstr, args) ->
        let ll = transl_list args in
        begin match cstr.cstr_tag with
          Cstr_constant n ->
            Lconst(Const_pointer n)
        | Cstr_block n ->
            begin try
              Lconst(Const_block(n, List.map extract_constant ll))
            with Not_constant ->
              Lprim(Pmakeblock(n, Immutable), ll)
            end
        | Cstr_exception path ->
            Lprim(Pmakeblock(0, Immutable), transl_path path :: ll)
        end
    | Texp_record ((lbl1, _) :: _ as lbl_expr_list) ->
        let lv = Array.create (Array.length lbl1.lbl_all) Lstaticfail in
        List.iter
          (fun (lbl, expr) -> lv.(lbl.lbl_pos) <- transl_exp expr)
          lbl_expr_list;
        let ll = Array.to_list lv in
        if List.exists (fun (lbl, expr) -> lbl.lbl_mut = Mutable) lbl_expr_list
        then begin
          match lbl1.lbl_repres with
            Record_regular -> Lprim(Pmakeblock(0, Mutable), ll)
          | Record_float -> Lprim(Pmakearray Pfloatarray, ll)
        end else begin
          try
            let cl = List.map extract_constant ll in
            match lbl1.lbl_repres with
              Record_regular -> Lconst(Const_block(0, cl))
            | Record_float ->
                Lconst(Const_float_array(List.map extract_float cl))
          with Not_constant ->
            match lbl1.lbl_repres with
              Record_regular -> Lprim(Pmakeblock(0, Immutable), ll)
            | Record_float -> Lprim(Pmakearray Pfloatarray, ll)
        end
    | Texp_field(arg, lbl) ->
        let access =
          match lbl.lbl_repres with
            Record_regular -> Pfield lbl.lbl_pos
          | Record_float -> Pfloatfield lbl.lbl_pos in
        Lprim(access, [transl_exp arg])
    | Texp_setfield(arg, lbl, newval) ->
        let access =
          match lbl.lbl_repres with
            Record_regular -> Psetfield(lbl.lbl_pos, maybe_pointer newval)
          | Record_float -> Psetfloatfield lbl.lbl_pos in
        Lprim(access, [transl_exp arg; transl_exp newval])
    | Texp_array expr_list ->
        let kind = array_kind e in
        let len = List.length expr_list in
        if len <= Config.max_young_wosize then
          Lprim(Pmakearray kind, transl_list expr_list)
        else begin
          let v = Ident.create "makearray" in
          let rec fill_fields pos = function
            [] ->
              Lvar v
          | arg :: rem ->
              Lsequence(Lprim(Parraysetu kind,
                              [Lvar v;
                               Lconst(Const_base(Const_int pos));
                               transl_exp arg]),
                        fill_fields (pos+1) rem) in
          Llet(Strict, v,
               Lprim(Pccall prim_makearray,
                     [Lconst(Const_base(Const_int len));
                      transl_exp (List.hd expr_list)]),
               fill_fields 1 (List.tl expr_list))
        end
    | Texp_ifthenelse(cond, ifso, Some ifnot) ->
        Lifthenelse(transl_exp cond,
                    event_before ifso (transl_exp ifso),
                    event_before ifnot (transl_exp ifnot))
    | Texp_ifthenelse(cond, ifso, None) ->
        Lifthenelse(transl_exp cond,
                    event_before ifso (transl_exp ifso),
                    lambda_unit)
    | Texp_sequence(expr1, expr2) ->
        Lsequence(transl_exp expr1, event_before expr2 (transl_exp expr2))
    | Texp_while(cond, body) ->
        Lwhile(transl_exp cond, event_before body (transl_exp body))
    | Texp_for(param, low, high, dir, body) ->
        Lfor(param, transl_exp low, transl_exp high, dir,
             event_before body (transl_exp body))
    | Texp_when(cond, body) ->
        event_before cond
          (Lifthenelse(transl_exp cond, event_before body (transl_exp body),
                       Lstaticfail))
  (*x: [[Translcore.transl_exp()]] cases *)
  | Texp_record_with (init_expr, ((lbl1, _) :: _ as lbl_expr_list)) ->

      let all_labels = lbl1.lbl_all in
      let lv = Array.create (Array.length all_labels) Lstaticfail in
      let init_id = Ident.create "init" in
      for i = 0 to Array.length all_labels - 1 do
        let access =
          match all_labels.(i).lbl_repres with
            Record_regular -> Pfield i
          | Record_float -> Pfloatfield i 
        in
        lv.(i) <- Lprim(access, [Lvar init_id])
      done;

      lbl_expr_list |> List.iter (fun (lbl, expr) -> 
        lv.(lbl.lbl_pos) <- transl_exp expr
      );
      let ll = Array.to_list lv in
      let mut =
        if List.exists (fun (lbl, expr) -> lbl.lbl_mut = Mutable) lbl_expr_list
        then Mutable
        else Immutable in
      let lam =
        try
          if mut = Mutable then raise Not_constant;
          let cl = List.map extract_constant ll in
          match lbl1.lbl_repres with
            Record_regular -> Lconst(Const_block(0, cl))
          | Record_float ->
              Lconst(Const_float_array(List.map extract_float cl))
        with Not_constant ->
          match lbl1.lbl_repres with
              Record_regular -> Lprim(Pmakeblock(0, mut), ll)
            | Record_float -> Lprim(Pmakearray Pfloatarray, ll) 
      in
      Llet(Strict, init_id, transl_exp init_expr, lam)
  (*e: [[Translcore.transl_exp()]] cases *)
  | _ ->
      fatal_error "Translcore.transl"
(*e: function [[Translcore.transl_exp]] *)

and transl_list expr_list =
  List.map transl_exp expr_list

and transl_cases pat_expr_list =
  List.map
    (fun (pat, expr) -> (pat, event_before expr (transl_exp expr)))
    pat_expr_list

and transl_tupled_cases patl_expr_list =
  List.map (fun (patl, expr) -> (patl, transl_exp expr)) patl_expr_list

(*s: function [[Translcore.transl_function]] *)
and transl_function loc untuplify_fn repr pat_expr_list =
  match pat_expr_list with
    [pat, ({exp_desc = Texp_function pl} as exp)] ->
      let param = name_pattern "param" pat_expr_list in
      let ((_, params), body) = transl_function exp.exp_loc false repr pl in
      ((Curried, param :: params),
       Matching.for_function loc None (Lvar param) [pat, body])
  | ({pat_desc = Tpat_tuple pl}, _) :: _ when untuplify_fn ->
      begin try
        let size = List.length pl in
        let pats_expr_list =
          List.map
            (fun (pat, expr) -> (Matching.flatten_pattern size pat, expr))
            pat_expr_list in
        let params = List.map (fun p -> Ident.create "param") pl in
        ((Tupled, params),
         Matching.for_tupled_function loc params
                                      (transl_tupled_cases pats_expr_list))
      with Matching.Cannot_flatten ->
        let param = name_pattern "param" pat_expr_list in
        ((Curried, [param]),
         Matching.for_function loc repr (Lvar param)
           (transl_cases pat_expr_list))
      end
  | _ ->
      let param = name_pattern "param" pat_expr_list in
      ((Curried, [param]),
       Matching.for_function loc repr (Lvar param)
         (transl_cases pat_expr_list))
(*e: function [[Translcore.transl_function]] *)

(*s: function [[Translcore.transl_let]] *)
and transl_let rec_flag pat_expr_list body =
  match rec_flag with
    Nonrecursive ->
      let rec transl = function
        [] ->
          body
      | (pat, expr) :: rem ->
          Matching.for_let pat.pat_loc (transl_exp expr) pat (transl rem)
      in transl pat_expr_list
  | Recursive ->
      let idlist =
        List.map
          (fun (pat, expr) -> 
            match pat.pat_desc with
              Tpat_var id -> id
            | _ -> raise(Error(pat.pat_loc, Illegal_letrec_pat)))
        pat_expr_list in
      let transl_case (pat, expr) id =
        let lam = transl_exp expr in
        if not (check_recursive_lambda idlist lam) then
          raise(Error(expr.exp_loc, Illegal_letrec_expr));
        (id, lam) in
      Lletrec(List.map2 transl_case pat_expr_list idlist, body)

(*e: function [[Translcore.transl_let]] *)

(*s: function [[Translcore.transl_exception]] *)
(* Compile an exception definition *)

let transl_exception id decl =
    Lprim(Pmakeblock(0, Immutable),
          [Lconst(Const_base(Const_string(Ident.name id)))])
(*e: function [[Translcore.transl_exception]] *)

(* Error report *)

open Format

(*s: constant [[Translcore.report_error]] *)
let report_error = function
    Illegal_letrec_pat ->
      print_string
      "Only variables are allowed as left-hand side of `let rec'"
  | Illegal_letrec_expr ->
      print_string
      "This kind of expression is not allowed as right-hand side of `let rec'"
(*e: constant [[Translcore.report_error]] *)
(*e: bytecomp/translcore.ml *)
