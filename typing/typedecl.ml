(*s: ./typing/typedecl.ml *)
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

(* Typing of type definitions *)

open Parsetree
open Types
open Typedtree
open Typetexp


(*s: type Typedecl.error *)
type error =
    Repeated_parameter
  | Duplicate_constructor of string
  | Too_many_constructors
  | Duplicate_label of string
  | Recursive_abbrev of string
  | Definition_mismatch of type_expr
(*e: type Typedecl.error *)

(*s: exception Typedecl.Error *)
exception Error of Location.t * error
(*e: exception Typedecl.Error *)

(*s: function Typedecl.enter_types *)
(* Enter all declared types in the environment as abstract types *)

let rec enter_types env = function
    [] ->
      ([], env)
  | (name, sdecl) :: srem ->
      let decl =
        { type_params = []; (*this field is unused when kind = Type_abstract*)
          type_arity = List.length sdecl.ptype_params;
          type_kind = Type_abstract;
          type_manifest = None } in
      let (id, extenv) = Env.enter_type name decl env in
      let (rem_id, final_env) = enter_types extenv srem in
      (id :: rem_id, final_env)
(*e: function Typedecl.enter_types *)

(* Translate one type declaration *)

module StringSet = Set

(*s: function Typedecl.transl_declaration *)
let transl_declaration env (name, sdecl) id =
  Ctype.begin_def();
  reset_type_variables();

  let params =
    try
      List.map enter_type_variable sdecl.ptype_params
    with Already_bound ->
      raise(Error(sdecl.ptype_loc, Repeated_parameter)) in

  let decl =
    { type_params = params;
      type_arity = List.length params;
      type_kind =
        (match sdecl.ptype_kind with
        (* boilerplate mapper Pxxx -> Xxx *)
          Ptype_abstract ->
            Type_abstract
        | Ptype_variant cstrs ->
            (*s: [[Typedecl.transl_declaration()]] sanity check when variant case *)
            let all_constrs = ref StringSet.empty in
            cstrs |> List.iter (fun (name, args) ->
                if StringSet.mem name !all_constrs then
                  raise(Error(sdecl.ptype_loc, Duplicate_constructor name));
                all_constrs := StringSet.add name !all_constrs
            );
            if List.length cstrs > Config.max_tag then
              raise(Error(sdecl.ptype_loc, Too_many_constructors));
            (*e: [[Typedecl.transl_declaration()]] sanity check when variant case *)
            Type_variant(List.map
              (fun (name, args) ->
                      (name, List.map (transl_simple_type env true) args))
              cstrs)
        | Ptype_record lbls ->
            (*s: [[Typedecl.transl_declaration()]] sanity check when record case *)
            let all_labels = ref StringSet.empty in
            lbls |> List.iter (fun (name, mut, arg) ->
                if StringSet.mem name !all_labels then
                  raise(Error(sdecl.ptype_loc, Duplicate_label name));
                all_labels := StringSet.add name !all_labels
            );
            (*e: [[Typedecl.transl_declaration()]] sanity check when record case *)
            Type_record(List.map
              (fun (name, mut, arg) ->
                      (name, mut, transl_simple_type env true arg))
              lbls)
        );
      type_manifest =
        (match sdecl.ptype_manifest with
        | None -> None
        | Some sty -> Some(transl_simple_type env true sty)
        ); 
  } in
  Ctype.end_def();

  List.iter Ctype.generalize params;

  (*s: [[Typedecl.transl_declaration()]] sanity check decl *)
  (* If both a variant/record definition and a type equation are given,
     need to check that the equation refers to a type of the same kind
     with the same constructors and labels *)
  (match decl with
    {type_kind = (Type_variant _ | Type_record _); type_manifest = Some ty} ->
      (match ty with
       | Tconstr(path, args) ->
          begin try
            let decl' = Env.find_type path env in
            if args = params & Includecore.type_declarations env id decl decl'
            then ()
            else raise(Error(sdecl.ptype_loc, Definition_mismatch ty))
          with Not_found ->
            raise(Error(sdecl.ptype_loc, Definition_mismatch ty))
          end
      | _ -> raise(Error(sdecl.ptype_loc, Definition_mismatch ty))
      )
  | _ -> ()
  );
  (*e: [[Typedecl.transl_declaration()]] sanity check decl *)
  (id, decl)
(*e: function Typedecl.transl_declaration *)

(*s: function Typedecl.check_recursive_abbrev *)
(* Check for recursive abbrevs *)

let check_recursive_abbrev env (name, sdecl) (id, decl) =
  match decl.type_manifest with
    Some ty ->
      if Ctype.free_type_ident env [id] ty
      then raise(Error(sdecl.ptype_loc, Recursive_abbrev name))
  | _ -> ()
(*e: function Typedecl.check_recursive_abbrev *)

(*s: function Typedecl.transl_type_decl *)
(* Translate a set of mutually recursive type declarations *)

let transl_type_decl env name_sdecl_list =
  Ctype.reset_def();
  let decls =
    match name_sdecl_list with
      [(name, {ptype_kind = Ptype_abstract}) as name_sdecl] ->
        (* No recursion involved, use original env for translation *)
        let id = Ident.create name in
        [transl_declaration env name_sdecl id]
    | _ ->
        (* Enter the types as abstract *)
        let (id_list, temp_env) = enter_types env name_sdecl_list in
        (* Translate each declaration *)
        List.map2 (transl_declaration temp_env) name_sdecl_list id_list 
  in
  (* Build the final env *)
  let newenv =
    List.fold_right
      (fun (id, decl) env -> Env.add_type id decl env)
      decls env in
  (*s: [[Typedecl.transl_type_decl()]] check recursive abbrevs *)
  (* Check for recursive abbrevs *)
  List.iter2 (check_recursive_abbrev newenv) name_sdecl_list decls;
  (* Done *)
  (*e: [[Typedecl.transl_type_decl()]] check recursive abbrevs *)
  (decls, newenv)
(*e: function Typedecl.transl_type_decl *)

(*s: function Typedecl.transl_exception *)
(* Translate an exception declaration *)

let transl_exception env excdecl =
  Ctype.reset_def();
  reset_type_variables();

  List.map (transl_simple_type env true) excdecl
(*e: function Typedecl.transl_exception *)

(*s: function Typedecl.transl_value_decl *)
(* Translate a value declaration *)

let transl_value_decl env valdecl =
  Ctype.reset_def();
  let ty = Typetexp.transl_type_scheme env valdecl.pval_type in
  { val_type = ty;
    val_prim = Primitive.parse_declaration (Ctype.arity ty) valdecl.pval_prim }
(*e: function Typedecl.transl_value_decl *)

(* Error report *)

open Format

(*s: function Typedecl.report_error *)
let report_error = function
    Repeated_parameter ->
      print_string "A type parameter occurs several times"
  | Duplicate_constructor s ->
      print_string "Two constructors are named "; print_string s
  | Too_many_constructors ->
      print_string "Too many constructors -- maximum is ";
      print_int Config.max_tag; print_string " constructors"
  | Duplicate_label s ->
      print_string "Two labels are named "; print_string s
  | Recursive_abbrev s ->
      print_string "The type abbreviation "; print_string s;
      print_string " is cyclic"
  | Definition_mismatch ty ->
      print_string
        "The variant or record definition does not match that of type";
      print_space(); Printtyp.type_expr ty
(*e: function Typedecl.report_error *)

(*e: ./typing/typedecl.ml *)
