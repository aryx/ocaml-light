(*s: ./typing/typemod.ml *)
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

(* Type-checking of the module language *)

open Misc
open Longident
open Path
open Parsetree
open Types
open Typedtree


(*s: type Typemod.error *)
type error =
    Unbound_module of Longident.t
  | Unbound_modtype of Longident.t
  | Not_included of Includemod.error list
  | Signature_expected
  | Structure_expected of module_type
  | With_no_component of Longident.t
  | With_mismatch of Longident.t * Includemod.error list
  | Repeated_name of string * string
  | Non_generalizable of type_expr
  | Non_generalizable_module of module_type
(*e: type Typemod.error *)

(*s: exception Typemod.Error *)
exception Error of Location.t * error
(*e: exception Typemod.Error *)

(*s: function Typemod.extract_sig_open *)
(* Extract a signature from a module type *)

let extract_sig_open env loc mty =
  match Mtype.scrape env mty with
    Tmty_signature sg -> sg
  | _ -> raise(Error(loc, Structure_expected mty))
(*e: function Typemod.extract_sig_open *)

(*s: function Typemod.type_module_path *)
(* Lookup the type of a module path *)

let type_module_path env loc lid =
  try
    Env.lookup_module lid env
  with Not_found ->
    raise(Error(loc, Unbound_module lid))
(*e: function Typemod.type_module_path *)

(* Check and translate a module type expression *)

let rec transl_modtype env smty =
  match smty.pmty_desc with
    Pmty_ident lid ->
      begin try
        let (path, info) = Env.lookup_modtype lid env in 
        Tmty_ident path
      with Not_found ->
        raise(Error(smty.pmty_loc, Unbound_modtype lid))
      end
  | Pmty_signature ssg ->
      Tmty_signature(transl_signature env ssg)
      
and transl_signature env sg =
  Ctype.init_def(Ident.current_time());
  match sg with
    [] -> []
  | {psig_desc = Psig_value(name, sdesc)} :: srem ->
      let desc = Typedecl.transl_value_decl env sdesc in
      let (id, newenv) = Env.enter_value name desc env in
      let rem = transl_signature newenv srem in
      Tsig_value(id, desc) :: rem
  | {psig_desc = Psig_type sdecls} :: srem ->
      let (decls, newenv) = Typedecl.transl_type_decl env sdecls in
      let rem = transl_signature newenv srem in
      map_end (fun (id, info) -> Tsig_type(id, info)) decls rem
  | {psig_desc = Psig_exception(name, sarg)} :: srem ->
      let arg = Typedecl.transl_exception env sarg in
      let (id, newenv) = Env.enter_exception name arg env in
      let rem = transl_signature newenv srem in
      Tsig_exception(id, arg) :: rem
  | {psig_desc = Psig_module(name, smty)} :: srem ->
      let mty = transl_modtype env smty in
      let (id, newenv) = Env.enter_module name mty env in
      let rem = transl_signature newenv srem in
      Tsig_module(id, mty) :: rem
  | {psig_desc = Psig_modtype(name, sinfo)} :: srem ->
      let info = transl_modtype_info env sinfo in
      let (id, newenv) = Env.enter_modtype name info env in
      let rem = transl_signature newenv srem in
      Tsig_modtype(id, info) :: rem
  | {psig_desc = Psig_open lid; psig_loc = loc} :: srem ->
      let (path, mty) = type_module_path env loc lid in
      let sg = extract_sig_open env loc mty in
      let newenv = Env.open_signature path sg env in
      transl_signature newenv srem

and transl_modtype_info env sinfo =
  match sinfo with
    Pmodtype_abstract ->
      Tmodtype_abstract
  | Pmodtype_manifest smty ->
      Tmodtype_manifest(transl_modtype env smty)

(* Check that all type and module identifiers in a structure have
   distinct names (so that access by named paths is unambiguous). *)

module StringSet = Set

(*s: function Typemod.check_unique_names *)
let check_unique_names sg =
  let type_names = ref StringSet.empty
  and module_names = ref StringSet.empty
  and modtype_names = ref StringSet.empty in
  let check cl loc set_ref name =
    if StringSet.mem name !set_ref
    then raise(Error(loc, Repeated_name(cl, name)))
    else set_ref := StringSet.add name !set_ref in
  let check_item item =
    match item.pstr_desc with
      Pstr_eval exp -> ()
    | Pstr_value(rec_flag, exps) -> ()
    | Pstr_primitive(name, desc) -> ()
    | Pstr_type name_decl_list ->
        List.iter
          (fun (name, decl) -> check "type" item.pstr_loc type_names name)
          name_decl_list
    | Pstr_exception(name, decl) -> ()
    | Pstr_module(name, smod) ->
        check "module" item.pstr_loc module_names name
    | Pstr_modtype(name, decl) ->
        check "module type" item.pstr_loc modtype_names name
    | Pstr_open lid -> ()
  in
    List.iter check_item sg
(*e: function Typemod.check_unique_names *)

(* Check that all core type schemes in a structure are closed *)

let rec closed_modtype = function
    Tmty_ident p -> true
  | Tmty_signature sg -> List.for_all closed_signature_item sg

and closed_signature_item = function
    Tsig_value(id, desc) -> Ctype.closed_schema desc.val_type
  | Tsig_module(id, mty) -> closed_modtype mty
  | _ -> true

(*s: function Typemod.check_nongen_scheme *)
let check_nongen_scheme env = function
    Tstr_value(rec_flag, pat_exp_list) ->
      List.iter
        (fun (pat, exp) ->
          if not (Ctype.closed_schema exp.exp_type) then
            raise(Error(exp.exp_loc, Non_generalizable exp.exp_type)))
        pat_exp_list
  | Tstr_module(id, md) ->
      if not (closed_modtype md.mod_type) then
        raise(Error(md.mod_loc, Non_generalizable_module md.mod_type))
  | _ -> ()
(*e: function Typemod.check_nongen_scheme *)

(*s: function Typemod.check_nongen_schemes *)
let check_nongen_schemes env str =
  List.iter (check_nongen_scheme env) str
(*e: function Typemod.check_nongen_schemes *)

(* Type a module value expression *)

let rec type_module env smod =
  match smod.pmod_desc with
    Pmod_ident lid ->
      let (path, mty) = type_module_path env smod.pmod_loc lid in
      { mod_desc = Tmod_ident path;
        mod_type = Mtype.strengthen env mty path;
        mod_env = env;
        mod_loc = smod.pmod_loc }
  | Pmod_structure sstr ->
      let (str, sg, finalenv) = type_structure env sstr in
      { mod_desc = Tmod_structure str;
        mod_type = Tmty_signature sg;
        mod_env = env;
        mod_loc = smod.pmod_loc }
  | Pmod_constraint(sarg, smty) ->
      let arg = type_module env sarg in
      let mty = transl_modtype env smty in
      let coercion =
        try
          Includemod.modtypes env arg.mod_type mty
        with Includemod.Error msg ->
          raise(Error(sarg.pmod_loc, Not_included msg)) in
      { mod_desc = Tmod_constraint(arg, mty, coercion);
        mod_type = mty;
        mod_env = env;
        mod_loc = smod.pmod_loc }

and type_structure env sstr =
  check_unique_names sstr;
  type_struct env sstr

and type_struct env sstr =
  Ctype.init_def(Ident.current_time());
  match sstr with
    [] ->
      ([], [], env)
  | {pstr_desc = Pstr_eval sexpr} :: srem ->
      let expr = Typecore.type_expression env sexpr in
      let (str_rem, sig_rem, final_env) = type_struct env srem in
      (Tstr_eval expr :: str_rem, sig_rem, final_env)
  | {pstr_desc = Pstr_value(rec_flag, sdefs)} :: srem ->
      let (defs, newenv) =
        Typecore.type_binding env rec_flag sdefs in
      let (str_rem, sig_rem, final_env) = type_struct newenv srem in
      let bound_idents = let_bound_idents defs in
      let make_sig_value id =
        Tsig_value(id, Env.find_value (Pident id) newenv) in
      (Tstr_value(rec_flag, defs) :: str_rem,
       map_end make_sig_value bound_idents sig_rem,
       final_env)
  | {pstr_desc = Pstr_primitive(name, sdesc)} :: srem ->
      let desc = Typedecl.transl_value_decl env sdesc in
      let (id, newenv) = Env.enter_value name desc env in
      let (str_rem, sig_rem, final_env) = type_struct newenv srem in
      (Tstr_primitive(id, desc) :: str_rem,
       Tsig_value(id, desc) :: sig_rem,
       final_env)
  | {pstr_desc = Pstr_type sdecls} :: srem ->
      let (decls, newenv) = Typedecl.transl_type_decl env sdecls in
      let (str_rem, sig_rem, final_env) = type_struct newenv srem in
      (Tstr_type decls :: str_rem,
       map_end (fun (id, info) -> Tsig_type(id, info)) decls sig_rem,
       final_env)
  | {pstr_desc = Pstr_exception(name, sarg)} :: srem ->
      let arg = Typedecl.transl_exception env sarg in
      let (id, newenv) = Env.enter_exception name arg env in
      let (str_rem, sig_rem, final_env) = type_struct newenv srem in
      (Tstr_exception(id, arg) :: str_rem,
       Tsig_exception(id, arg) :: sig_rem,
       final_env)
  | {pstr_desc = Pstr_module(name, smodl)} :: srem ->
      let modl = type_module env smodl in
      let (id, newenv) = Env.enter_module name modl.mod_type env in
      let (str_rem, sig_rem, final_env) = type_struct newenv srem in
      (Tstr_module(id, modl) :: str_rem,
       Tsig_module(id, modl.mod_type) :: sig_rem,
       final_env)
  | {pstr_desc = Pstr_modtype(name, smty)} :: srem ->
      let mty = transl_modtype env smty in
      let (id, newenv) = Env.enter_modtype name (Tmodtype_manifest mty) env in
      let (str_rem, sig_rem, final_env) = type_struct newenv srem in
      (Tstr_modtype(id, mty) :: str_rem,
       Tsig_modtype(id, Tmodtype_manifest mty) :: sig_rem,
       final_env)
  | {pstr_desc = Pstr_open lid; pstr_loc = loc} :: srem ->
      let (path, mty) = type_module_path env loc lid in
      let sg = extract_sig_open env loc mty in
      type_struct (Env.open_signature path sg env) srem

(* Error report *)

open Format
open Printtyp

(*s: constant Typemod.report_error *)
let report_error = function
    Unbound_module lid ->
      print_string "Unbound module "; longident lid
  | Unbound_modtype lid ->
      print_string "Unbound module type "; longident lid
  | Not_included errs ->
      open_vbox 0;
      print_string "Signature mismatch:"; print_space();
      Includemod.report_error errs;
      close_box()
  | Signature_expected ->
      print_string "This module type is not a signature"
  | Structure_expected mty ->
      open_box 0;
      print_string "This module is not a structure; it has type";
      print_space(); modtype mty;
      close_box()
  | With_no_component lid ->
      open_box 0;
      print_string "The signature constrained by `with' has no component named";
      print_space(); longident lid;
      close_box()
  | With_mismatch(lid, explanation) ->
      open_vbox 0;
      open_box 0;
      print_string "In this `with' constraint, the new definition of";
      print_space(); longident lid; print_space();
      print_string "does not match its original definition";
      print_space(); print_string "in the constrained signature:";
      close_box();
      print_space();
      Includemod.report_error explanation;
      close_box()
  | Repeated_name(kind, name) ->
      open_box 0;
      print_string "Multiple definition of the "; print_string kind;
      print_string " name "; print_string name; print_string ".";
      print_space();
      print_string "Names must be unique in a given structure.";
      close_box()
  | Non_generalizable typ ->
      open_box 0;
      print_string "The type of this expression,"; print_space();
      type_scheme typ; print_string ","; print_space();
      print_string "contains type variables that cannot be generalized";
      close_box()
  | Non_generalizable_module mty ->
      open_box 0;
      print_string "The type of this module,"; print_space();
      modtype mty; print_string ","; print_space();
      print_string "contains type variables that cannot be generalized";
      close_box()
(*e: constant Typemod.report_error *)
(*e: ./typing/typemod.ml *)
