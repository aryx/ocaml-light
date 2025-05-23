(*s: typing/typecore.ml *)
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

(* Typechecking for the core language *)

open Misc
open Asttypes
open Parsetree
open Types
open Typedtree
open Ctype


(*s: type [[Typecore.error]] *)
type error =
    Unbound_value       of Longident.t
  | Unbound_constructor of Longident.t
  | Unbound_label       of Longident.t

  | Constructor_arity_mismatch of Longident.t * int * int
  | Label_mismatch of Longident.t * type_expr * type_expr
  | Pattern_type_clash of type_expr * type_expr

  | Multiply_bound_variable
  | Orpat_not_closed
  | Expr_type_clash of type_expr * type_expr
  | Apply_non_function of type_expr

  | Label_multiply_defined of Longident.t
  | Label_missing
  | Label_not_mutable of Longident.t

  | Bad_format of string

  | Too_many_arguments
(*e: type [[Typecore.error]] *)

(*s: exception [[Typecore.Error]] *)
exception Error of Location.t * error
(*e: exception [[Typecore.Error]] *)

(*s: function [[Typecore.type_constant]] *)
(* Typing of constants *)

let type_constant = function
    Const_int _ -> Predef.type_int
  | Const_char _ -> Predef.type_char
  | Const_string _ -> Predef.type_string
  | Const_float _ -> Predef.type_float
(*e: function [[Typecore.type_constant]] *)

(*s: function [[Typecore.unify_pat]] *)
(* Typing of patterns *)

let unify_pat env pat expected_ty =
  try
    unify env pat.pat_type expected_ty
  with Unify ->
    raise(Error(pat.pat_loc, Pattern_type_clash(pat.pat_type, expected_ty)))
(*e: function [[Typecore.unify_pat]] *)

(*s: constant [[Typecore.pattern_variables]] *)
let pattern_variables = ref ([]: (Ident.t * type_expr) list)
(*e: constant [[Typecore.pattern_variables]] *)

(*s: function [[Typecore.enter_variable]] *)
let enter_variable loc name ty =
  if List.exists (fun (id, _ty) -> Ident.name id = name) !pattern_variables
  then raise(Error(loc, Multiply_bound_variable));
  let id = Ident.create name in
  pattern_variables := (id, ty) :: !pattern_variables;
  id
(*e: function [[Typecore.enter_variable]] *)

(*s: function [[Typecore.type_pat]] *)
let rec type_pat env sp =
  match sp.ppat_desc with
    Ppat_any ->
      { pat_desc = Tpat_any;
        pat_loc = sp.ppat_loc;
        pat_type = newvar() }
  | Ppat_var name ->
      let ty = newvar() in
      let id = enter_variable sp.ppat_loc name ty in
      { pat_desc = Tpat_var id;
        pat_loc = sp.ppat_loc;
        pat_type = ty }
  | Ppat_alias(sp, name) ->
      let p = type_pat env sp in
      let id = enter_variable sp.ppat_loc name p.pat_type in
      { pat_desc = Tpat_alias(p, id);
        pat_loc = sp.ppat_loc;
        pat_type = p.pat_type }
  | Ppat_constant cst ->
      { pat_desc = Tpat_constant cst;
        pat_loc = sp.ppat_loc;
        pat_type = type_constant cst }
  | Ppat_tuple spl ->
      let pl = List.map (type_pat env) spl in
      { pat_desc = Tpat_tuple pl;
        pat_loc = sp.ppat_loc;
        pat_type = Ttuple(List.map (fun p -> p.pat_type) pl) }
  | Ppat_construct(lid, sarg) ->
      let constr =
        try
          Env.lookup_constructor lid env
        with Not_found ->
          raise(Error(sp.ppat_loc, Unbound_constructor lid)) in
      let sargs =
        match sarg with
          None -> []
        | Some {ppat_desc = Ppat_tuple spl} when constr.cstr_arity > 1 -> spl
        | Some({ppat_desc = Ppat_any} as sp) when constr.cstr_arity > 1 ->
            replicate_list sp constr.cstr_arity
        | Some sp -> [sp] in
      if List.length sargs <> constr.cstr_arity then
        raise(Error(sp.ppat_loc, Constructor_arity_mismatch(lid,
                                     constr.cstr_arity, List.length sargs)));
      let args = List.map (type_pat env) sargs in
      let (ty_args, ty_res) = instance_constructor constr in
      List.iter2 (unify_pat env) args ty_args;
      { pat_desc = Tpat_construct(constr, args);
        pat_loc = sp.ppat_loc;
        pat_type = ty_res }
  | Ppat_record lid_sp_list ->
      let ty = newvar() in
      let type_label_pat (lid, sarg) =
        let label =
          try
            Env.lookup_label lid env
          with Not_found ->
            raise(Error(sp.ppat_loc, Unbound_label lid)) in
        let (ty_arg, ty_res) = instance_label label in
        begin try
          unify env ty_res ty
        with Unify ->
          raise(Error(sp.ppat_loc, Label_mismatch(lid, ty_res, ty)))
        end;
        let arg = type_pat env sarg in
        unify_pat env arg ty_arg;
        (label, arg)
      in
      { pat_desc = Tpat_record(List.map type_label_pat lid_sp_list);
        pat_loc = sp.ppat_loc;
        pat_type = ty }
  | Ppat_or(sp1, sp2) ->
      let initial_pattern_variables = !pattern_variables in
      let p1 = type_pat env sp1 in
      let p2 = type_pat env sp2 in
      if !pattern_variables != initial_pattern_variables then
        raise(Error(sp.ppat_loc, Orpat_not_closed));
      unify_pat env p2 p1.pat_type;
      { pat_desc = Tpat_or(p1, p2);
        pat_loc = sp.ppat_loc;
        pat_type = p1.pat_type }
  | Ppat_constraint(sp, sty) ->
      let p = type_pat env sp in
      let ty = Typetexp.transl_simple_type env false sty in
      unify_pat env p ty;
      p
(*e: function [[Typecore.type_pat]] *)

(*s: function [[Typecore.add_pattern_variables]] *)
let add_pattern_variables env =
  let pv = !pattern_variables in
  pattern_variables := [];
  List.fold_right
    (fun (id, ty) env ->
      Env.add_value id {val_type = ty; val_prim = None} env)
    pv env
(*e: function [[Typecore.add_pattern_variables]] *)

(*s: function [[Typecore.type_pattern]] *)
let type_pattern env spat =
  pattern_variables := [];
  let pat = type_pat env spat in
  let new_env = add_pattern_variables env in
  (pat, new_env)
(*e: function [[Typecore.type_pattern]] *)

(*s: function [[Typecore.type_pattern_list]] *)
let type_pattern_list env spatl =
  pattern_variables := [];
  let patl = List.map (type_pat env) spatl in
  let new_env = add_pattern_variables env in
  (patl, new_env)
(*e: function [[Typecore.type_pattern_list]] *)

(*s: function [[Typecore.is_nonexpansive]] *)
(* Generalization criterion for expressions *)

let rec is_nonexpansive exp =
  match exp.exp_desc with
  | Texp_record lbl_exp_list ->
      lbl_exp_list |> List.for_all (fun (lbl, exp) -> 
        lbl.lbl_mut = Immutable && is_nonexpansive exp
       )
  | Texp_ident(_,_) -> true
  | Texp_constant _ -> true
  | Texp_let(_rec_flag, pat_exp_list, body) ->
      List.for_all (fun (_pat, exp) -> is_nonexpansive exp) pat_exp_list &&
      is_nonexpansive body
  | Texp_function _ -> true
  | Texp_tuple el ->
      List.for_all is_nonexpansive el
  | Texp_construct(_, el) ->
      List.for_all is_nonexpansive el
              
  | Texp_field(exp, _lbl) -> is_nonexpansive exp
  | Texp_array [] -> true

  (*s: [[Typecore.is_nonexpansive()]] extra cases *)
  | Texp_record_with (exp, lbl_exp_list) ->
      lbl_exp_list |> List.for_all (fun (lbl, exp) -> 
        lbl.lbl_mut = Immutable && is_nonexpansive exp
       ) &&
      is_nonexpansive exp
  (*e: [[Typecore.is_nonexpansive()]] extra cases *)

  | _ -> false
(*e: function [[Typecore.is_nonexpansive]] *)

(*s: function [[Typecore.type_format]] *)
(* Typing of printf formats *)

let type_format loc fmt =
  let len = String.length fmt in
  let ty_input = newvar() in
  let ty_result = newvar() in
  let rec skip_args j =
    if j >= len then j else
      match fmt.[j] with
        '0' .. '9' | ' ' | '.' | '-' -> skip_args (j+1)
      | _ -> j in
  let rec scan_format i =
    if i >= len 
    then ty_result 
    else
     match fmt.[i] with
      '%' ->
        let j = skip_args(i+1) in
        (match String.unsafe_get fmt j with
        (* We're using unsafe_get here so that if j = String.length fmt,
           we'll fall in the catch-all case of the match *)
          '%' | '!' ->
            scan_format (j+1)
        | 's' ->
            Tarrow(Predef.type_string, scan_format (j+1))
        | 'c' ->
            Tarrow(Predef.type_char, scan_format (j+1))
        | 'd' | 'o' | 'x' | 'X' | 'u' ->
            Tarrow(Predef.type_int, scan_format (j+1))
        | 'f' | 'e' | 'E' | 'g' | 'G' ->
            Tarrow(Predef.type_float, scan_format (j+1))
        | 'b' ->
            Tarrow(Predef.type_bool, scan_format (j+1))
        | 'a' ->
            let ty_arg = newvar() in
            Tarrow (Tarrow(ty_input, Tarrow (ty_arg, ty_result)),
                    Tarrow (ty_arg, scan_format (j+1)))
        | 't' ->
            Tarrow(Tarrow(ty_input, ty_result), scan_format (j+1))
        | _c ->
            raise(Error(loc, Bad_format(String.sub fmt i (j-i))))
        )
    | _ -> scan_format (i+1) in
  Tconstr(Predef.path_format, [scan_format 0; ty_input; ty_result])
(*e: function [[Typecore.type_format]] *)

(*s: function [[Typecore.unify_exp]] *)
(* Typing of expressions *)

let unify_exp env exp expected_ty =
  try
    unify env exp.exp_type expected_ty
  with Unify ->
    raise(Error(exp.exp_loc, Expr_type_clash(exp.exp_type, expected_ty)))
(*e: function [[Typecore.unify_exp]] *)

(*s: function [[Typecore.type_exp]] *)
let rec type_exp env sexp =
  match sexp.pexp_desc with
  (*s: [[Typecore.type_exp()]] match cases *)
  | Pexp_constant cst ->
      { exp_desc = Texp_constant cst;
        exp_loc = sexp.pexp_loc;
        exp_type = type_constant cst }
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_construct(lid, sarg) ->
      let constr =
        try
          Env.lookup_constructor lid env
        with Not_found ->
          raise(Error(sexp.pexp_loc, Unbound_constructor lid)) 
      in
      let sargs =
        match sarg with
          None -> []
        | Some {pexp_desc = Pexp_tuple sel} when constr.cstr_arity > 1 -> sel
        | Some se -> [se] 
      in
      (*s: [[Typecore.type_exp()]] constructor case, sanity check *)
      if List.length sargs <> constr.cstr_arity 
      then
        raise(Error(sexp.pexp_loc, Constructor_arity_mismatch(lid,
                                       constr.cstr_arity, List.length sargs)));
      (*e: [[Typecore.type_exp()]] constructor case, sanity check *)

      let (ty_args, ty_res) = instance_constructor constr in
      let args = List.map2 (type_expect env) sargs ty_args in
      { exp_desc = Texp_construct(constr, args);
        exp_loc = sexp.pexp_loc;
        exp_type = ty_res }
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_record lid_sexp_list ->
      let ty = newvar() in
      let num_fields = ref 0 in
      let type_label_exp (lid, sarg) =
        let label =
          try
            Env.lookup_label lid env
          with Not_found ->
            raise(Error(sexp.pexp_loc, Unbound_label lid)) 
        in
        let (ty_arg, ty_res) = instance_label label in
        (try
          unify env ty_res ty
        with Unify ->
          raise(Error(sexp.pexp_loc, Label_mismatch(lid, ty_res, ty)))
        );
        let arg = type_expect env sarg ty_arg in
        num_fields := Array.length label.lbl_all;
        (label, arg) 
      in
      let lbl_exp_list = List.map type_label_exp lid_sexp_list in
      (*s: [[Typecore.type_exp()]] record case, sanity check duplicates and missing *)
      let rec check_duplicates = function
        [] -> ()
      | (lid, _sarg) :: remainder ->
          if List.mem_assoc lid remainder
          then raise(Error(sexp.pexp_loc, Label_multiply_defined lid))
          else check_duplicates remainder 
      in
      check_duplicates lid_sexp_list;

      if List.length lid_sexp_list <> !num_fields 
      then raise(Error(sexp.pexp_loc, Label_missing));
      (*e: [[Typecore.type_exp()]] record case, sanity check duplicates and missing *)
      { exp_desc = Texp_record lbl_exp_list;
        exp_loc = sexp.pexp_loc;
        exp_type = ty }
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_field(sarg, lid) ->
      let arg = type_exp env sarg in
      let label =
        try
          Env.lookup_label lid env
        with Not_found ->
          raise(Error(sexp.pexp_loc, Unbound_label lid)) 
      in
      let (ty_arg, ty_res) = instance_label label in
      unify_exp env arg ty_res;
      { exp_desc = Texp_field(arg, label);
        exp_loc = sexp.pexp_loc;
        exp_type = ty_arg }
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_setfield(srecord, lid, snewval) ->
      let record = type_exp env srecord in
      let label =
        try
          Env.lookup_label lid env
        with Not_found ->
          raise(Error(sexp.pexp_loc, Unbound_label lid)) 
      in
      if label.lbl_mut = Immutable 
      then raise(Error(sexp.pexp_loc, Label_not_mutable lid));
      let (ty_arg, ty_res) = instance_label label in
      unify_exp env record ty_res;
      let newval = type_expect env snewval ty_arg in
      { exp_desc = Texp_setfield(record, label, newval);
        exp_loc = sexp.pexp_loc;
        exp_type = Predef.type_unit }
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_tuple sexpl ->
      let expl = List.map (type_exp env) sexpl in
      { exp_desc = Texp_tuple expl;
        exp_loc = sexp.pexp_loc;
        exp_type = Ttuple(List.map (fun exp -> exp.exp_type) expl) }
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_sequence(sexp1, sexp2) ->
      let exp1 = type_statement env sexp1 in
      let exp2 = type_exp env sexp2 in
      { exp_desc = Texp_sequence(exp1, exp2);
        exp_loc = sexp.pexp_loc;
        exp_type = exp2.exp_type }
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_ifthenelse(scond, sifso, sifnot) ->
      let cond = type_expect env scond Predef.type_bool in
      (match sifnot with
      | None ->
          let ifso = type_expect env sifso Predef.type_unit in
          { exp_desc = Texp_ifthenelse(cond, ifso, None);
            exp_loc = sexp.pexp_loc;
            exp_type = Predef.type_unit }
      | Some sexp ->
          let ifso = type_exp env sifso in
          let ifnot = type_expect env sexp ifso.exp_type in
          { exp_desc = Texp_ifthenelse(cond, ifso, Some ifnot);
            exp_loc = sexp.pexp_loc;
            exp_type = ifso.exp_type }
      )
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_while(scond, sbody) ->
      let cond = type_expect env scond Predef.type_bool in
      let body = type_statement env sbody in
      { exp_desc = Texp_while(cond, body);
        exp_loc = sexp.pexp_loc;
        exp_type = Predef.type_unit }
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_for(param, slow, shigh, dir, sbody) ->
      let low = type_expect env slow Predef.type_int in
      let high = type_expect env shigh Predef.type_int in
      let (id, new_env) =
        Env.enter_value param {val_type = Predef.type_int;
                                val_prim = None} env 
      in
      let body = type_statement new_env sbody in
      { exp_desc = Texp_for(id, low, high, dir, body);
        exp_loc = sexp.pexp_loc;
        exp_type = Predef.type_unit }
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_match(sarg, caselist) ->
      let arg = type_exp env sarg in
      let ty_res = newvar() in
      let cases = type_cases env arg.exp_type ty_res caselist in
      (*s: [[Typecore.type_exp()]] match case, sanity check unused or partial *)
      Parmatch.check_unused cases;
      Parmatch.check_partial sexp.pexp_loc cases;
      (*e: [[Typecore.type_exp()]] match case, sanity check unused or partial *)
      { exp_desc = Texp_match(arg, cases);
        exp_loc = sexp.pexp_loc;
        exp_type = ty_res }
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_when(scond, sbody) ->
      let cond = type_expect env scond Predef.type_bool in
      let body = type_exp env sbody in
      { exp_desc = Texp_when(cond, body);
        exp_loc = sexp.pexp_loc;
        exp_type = body.exp_type }
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_function caselist ->
      let ty_arg = newvar() in
      let ty_res = newvar() in
      let cases = type_cases env ty_arg ty_res caselist in
      (*s: [[Typecore.type_exp()]] match case, sanity check unused or partial *)
      Parmatch.check_unused cases;
      Parmatch.check_partial sexp.pexp_loc cases;
      (*e: [[Typecore.type_exp()]] match case, sanity check unused or partial *)
      { exp_desc = Texp_function cases;
        exp_loc = sexp.pexp_loc;
        exp_type = Tarrow(ty_arg, ty_res) }
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_apply(sfunct, sargs) ->
      let funct = type_exp env sfunct in

      let rec type_args ty_fun = function
      | [] ->
          ([], ty_fun)
      | sarg1 :: sargl ->
          let (ty1, ty2) =
            try
              filter_arrow env ty_fun
            with Unify ->
              raise(Error(sfunct.pexp_loc,
                          Apply_non_function funct.exp_type)) 
          in
          let arg1 = type_expect env sarg1 ty1 in
          let (argl, ty_res) = type_args ty2 sargl in
          (arg1 :: argl, ty_res) 
      in

      let (args, ty_res) = type_args funct.exp_type sargs in
      { exp_desc = Texp_apply(funct, args);
        exp_loc = sexp.pexp_loc;
        exp_type = ty_res }
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_constraint(sarg, sty) ->
      let ty = Typetexp.transl_simple_type env false sty in
      let arg = type_expect env sarg ty in
      { exp_desc = arg.exp_desc;
        exp_loc = arg.exp_loc;
        exp_type = ty }
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_let(rec_flag, spat_sexp_list, sbody) ->
      let (pat_exp_list, new_env) = type_let env rec_flag spat_sexp_list in
      let body = type_exp new_env sbody in
      { exp_desc = Texp_let(rec_flag, pat_exp_list, body);
        exp_loc = sexp.pexp_loc;
        exp_type = body.exp_type }
  (*x: [[Typecore.type_exp()]] match cases *)
  |  Pexp_ident lid ->
      begin try
        let (path, desc) = Env.lookup_value lid env in
        { exp_desc = Texp_ident(path, desc);
          exp_loc = sexp.pexp_loc;
          exp_type = instance desc.val_type }
      with Not_found ->
        raise(Error(sexp.pexp_loc, Unbound_value lid))
      end
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_try(sbody, caselist) ->
      let body = type_exp env sbody in
      let cases = type_cases env Predef.type_exn body.exp_type caselist in
      Parmatch.check_unused cases;
      { exp_desc = Texp_try(body, cases);
        exp_loc = sexp.pexp_loc;
        exp_type = body.exp_type }
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_record_with (sexp, lid_sexp_list) ->
      let ty = newvar() in
      let num_fields = ref 0 in
      let type_label_exp (lid, sarg) =
        let label =
          try
            Env.lookup_label lid env
          with Not_found ->
            raise(Error(sexp.pexp_loc, Unbound_label lid)) 
        in
        let (ty_arg, ty_res) = instance_label label in
        (try
          unify env ty_res ty
        with Unify ->
          raise(Error(sexp.pexp_loc, Label_mismatch(lid, ty_res, ty)))
        );
        let arg = type_expect env sarg ty_arg in
        num_fields := Array.length label.lbl_all;
        (label, arg) 
      in
      let lbl_exp_list = List.map type_label_exp lid_sexp_list in

      let rec check_duplicates = function
        [] -> ()
      | (lid, _sarg) :: remainder ->
          if List.mem_assoc lid remainder
          then raise(Error(sexp.pexp_loc, Label_multiply_defined lid))
          else check_duplicates remainder 
      in
      check_duplicates lid_sexp_list;

      let exp = type_expect env sexp ty in

      { exp_desc = Texp_record_with (exp, lbl_exp_list);
        exp_loc = sexp.pexp_loc;
        exp_type = ty }
  (*x: [[Typecore.type_exp()]] match cases *)
  | Pexp_array(sargl) ->
      let ty = newvar() in
      let argl = List.map (fun sarg -> type_expect env sarg ty) sargl in
      { exp_desc = Texp_array argl;
        exp_loc = sexp.pexp_loc;
        exp_type = Predef.type_array ty }
  (*e: [[Typecore.type_exp()]] match cases *)
(*e: function [[Typecore.type_exp]] *)

(*s: function [[Typecode.type_expect]] *)
(* Typing of an expression with an expected type.
   Some constructs are treated specially to provide better error messages. *)

and type_expect env sexp ty_expected =
  match sexp.pexp_desc with
  (*s: [[Typecode.type_expect()]] match cases *)
  | Pexp_sequence(sexp1, sexp2) ->
      let exp1 = type_statement env sexp1 in
      let exp2 = type_expect env sexp2 ty_expected in
      { exp_desc = Texp_sequence(exp1, exp2);
        exp_loc = sexp.pexp_loc;
        exp_type = exp2.exp_type }
  (*x: [[Typecode.type_expect()]] match cases *)
  | Pexp_let(rec_flag, spat_sexp_list, sbody) ->
      let (pat_exp_list, new_env) = type_let env rec_flag spat_sexp_list in
      let body = type_expect new_env sbody ty_expected in
      { exp_desc = Texp_let(rec_flag, pat_exp_list, body);
        exp_loc = sexp.pexp_loc;
        exp_type = body.exp_type }
  (*x: [[Typecode.type_expect()]] match cases *)
  |  Pexp_constant(Const_string s as cst) ->
      let exp =
        { exp_desc = Texp_constant cst;
          exp_loc = sexp.pexp_loc;
          exp_type =
            (* Terrible hack for format strings *)
            match Ctype.repr ty_expected with
              Tconstr(path, _) when Path.same path Predef.path_format ->
                type_format sexp.pexp_loc s
            | _ -> Predef.type_string } in
      unify_exp env exp ty_expected;
      exp
  (*e: [[Typecode.type_expect()]] match cases *)
  | Pexp_function caselist ->
      let (ty_arg, ty_res) =
        try filter_arrow env ty_expected with Unify ->
          raise(Error(sexp.pexp_loc, Too_many_arguments))
      in
      let cases =
        List.map
          (fun (spat, sexp) ->
             let (pat, ext_env) = type_pattern env spat in
             unify_pat env pat ty_arg;
             let exp = type_expect ext_env sexp ty_res in
             (pat, exp))
          caselist
      in
      Parmatch.check_unused cases;
      Parmatch.check_partial sexp.pexp_loc cases;
      { exp_desc = Texp_function cases;
        exp_loc = sexp.pexp_loc;
        exp_type = Tarrow(ty_arg, ty_res);
      }
  | _ ->
      let exp = type_exp env sexp in
      unify_exp env exp ty_expected;
      exp
(*e: function [[Typecode.type_expect]] *)

(*s: function [[Typecore.type_statement]] *)
(* Typing of statements (expressions whose values are discarded) *)

and type_statement env sexp =
    let exp = type_exp env sexp in
    match Ctype.repr exp.exp_type with
      Tarrow(_, _) ->
        Location.print_warning sexp.pexp_loc
          "this function application is partial,\n\
           maybe some arguments are missing.";
        exp
    | _ -> exp
(*e: function [[Typecore.type_statement]] *)

(*s: function [[Typecore.type_cases]] *)
(* Typing of match cases *)

and type_cases env ty_arg ty_res caselist =
  List.map (fun (spat, sexp) ->
      let (pat, ext_env) = type_pattern env spat in
      unify_pat env pat ty_arg;
      let exp = type_expect ext_env sexp ty_res in
      (pat, exp)
  ) caselist
(*e: function [[Typecore.type_cases]] *)

(*s: function [[Typecode.type_let]] *)
(* Typing of let bindings *)

and type_let env rec_flag spat_sexp_list =

  (*s: [[Typecode.type_let()]] before typing *)
  begin_def();
  (*e: [[Typecode.type_let()]] before typing *)
  let (pat_list, new_env) =
    spat_sexp_list 
    |> List.map (fun (spat, _sexp) -> spat)
    |> type_pattern_list env 
  in
  let exp_env =
    match rec_flag with 
    | Nonrecursive -> env 
    | Recursive -> new_env 
  in
  let exp_list =
    List.map2
      (fun (_spat, sexp) pat -> type_expect exp_env sexp pat.pat_type)
      spat_sexp_list pat_list in
  (*s: [[Typecode.type_let()]] sanity check partial patterns *)
  List.iter2
    (fun pat exp -> Parmatch.check_partial pat.pat_loc [pat, exp])
    pat_list exp_list;
  (*e: [[Typecode.type_let()]] sanity check partial patterns *)
  (*s: [[Typecode.type_let()]] after typing *)
  end_def();
  (*e: [[Typecode.type_let()]] after typing *)
  (*s: [[Typecode.type_let()]] after typing, generalize or not *)
  (*s: [[Typecode.type_let()]] generalization criteria action *)
  exp_list |> List.iter (fun exp -> 
    if not (is_nonexpansive exp) 
    then Ctype.make_nongen exp.exp_type
  );
  (*e: [[Typecode.type_let()]] generalization criteria action *)
  exp_list |> List.iter (fun exp -> 
    generalize exp.exp_type);
  (*e: [[Typecode.type_let()]] after typing, generalize or not *)

  (List.combine pat_list exp_list, new_env)
(*e: function [[Typecode.type_let]] *)

(*s: function [[Typecore.type_binding]] *)
(* Typing of toplevel bindings *)

let type_binding env rec_flag spat_sexp_list =
  reset_def();
  Typetexp.reset_type_variables();

  type_let env rec_flag spat_sexp_list
(*e: function [[Typecore.type_binding]] *)

(*s: function [[Typecore.type_expression]] *)
(* Typing of toplevel expressions *)

let type_expression env sexp =
  (*s: [[Typecore.type_expression()]] before [[type_exp]] *)
  (*s: [[Typecore.type_expression()]] before [[begin_def]], reset *)
  reset_def();
  Typetexp.reset_type_variables();
  (*e: [[Typecore.type_expression()]] before [[begin_def]], reset *)
  begin_def();
  (*e: [[Typecore.type_expression()]] before [[type_exp]] *)
  let exp = type_exp env sexp in
  (*s: [[Typecore.type_expression()]] after [[type_exp]] *)
  end_def();
  (*e: [[Typecore.type_expression()]] after [[type_exp]] *)
  (*s: [[Typecore.type_expression()]] after [[type_exp]], generalize or not *)
  if 
    (*s: [[Typecore.type_expression()]] generalization criteria *)
    is_nonexpansive exp 
    (*e: [[Typecore.type_expression()]] generalization criteria *)
  then generalize exp.exp_type;
  (*e: [[Typecore.type_expression()]] after [[type_exp]], generalize or not *)

  exp
(*e: function [[Typecore.type_expression]] *)

(* Error report *)

open Format
open Printtyp

(*s: function [[Typecore.report_error]] *)
let report_error = function
    Unbound_value lid ->
      print_string "Unbound value "; longident lid
  | Unbound_constructor lid ->
      print_string "Unbound constructor "; longident lid
  | Unbound_label lid ->
      print_string "Unbound label "; longident lid
  | Constructor_arity_mismatch(lid, expected, provided) ->
      open_hovbox 0;
      print_string "The constructor "; longident lid;
      print_space(); print_string "expects "; print_int expected;
      print_string " argument(s),"; print_space();
      print_string "but is here applied to "; print_int provided;
      print_string " argument(s)";
      close_box()
  | Label_mismatch(lid, actual, expected) ->
      open_hovbox 0;
      print_string "The label "; longident lid;
      print_space(); print_string "belongs to the type"; print_space();
      type_expr actual; print_space();
      print_string "but is here mixed with labels of type"; print_space();
      type_expr expected;
      close_box()
  | Pattern_type_clash(inferred, expected) ->
      open_hovbox 0;
      print_string "This pattern matches values of type"; print_space();
      type_expr inferred; print_space();
      print_string "but is here used to match values of type"; print_space();
      type_expr expected;
      close_box()
  | Multiply_bound_variable ->
      print_string "This variable is bound several times in this matching"
  | Orpat_not_closed ->
      print_string "A pattern with | must not bind variables"
  | Expr_type_clash(inferred, expected) ->
      open_hovbox 0;
      print_string "This expression has type"; print_space();
      type_expr inferred; print_space();
      print_string "but is here used with type"; print_space();
      type_expr expected;
      close_box()
  | Apply_non_function typ ->
      begin match Ctype.repr typ with
        Tarrow(_, _) ->
          print_string "This function is applied to too many arguments"
      | _ ->
          print_string "This expression is not a function, it cannot be applied"
      end
  | Label_multiply_defined lid ->
      print_string "The label "; longident lid;
      print_string " is defined several times"
  | Label_missing ->
      print_string "Some labels are undefined"
  | Label_not_mutable lid ->
      print_string "The label "; longident lid;
      print_string " is not mutable"
  | Bad_format s ->
      print_string "Bad format `"; print_string s; print_string "'"
  | Too_many_arguments ->
      print_string "This function has too many arguments"
(*e: function [[Typecore.report_error]] *)
(*e: typing/typecore.ml *)
