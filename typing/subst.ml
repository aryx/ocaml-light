(*s: ./typing/subst.ml *)
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

(* Substitutions *)

open Misc
open Path
open Types


(*s: type Subst.t *)
type t = 
  { types: Path.t Ident.tbl;
    modules: Path.t Ident.tbl;
    modtypes: module_type Ident.tbl }
(*e: type Subst.t *)

(*s: constant Subst.identity *)
let identity =
  { types = Ident.empty; modules = Ident.empty; modtypes = Ident.empty }
(*e: constant Subst.identity *)

(*s: function Subst.add_type *)
let add_type id p s =
  { types = Ident.add id p s.types;
    (* boilerplate, with record *)
    modules = s.modules;
    modtypes = s.modtypes }
(*e: function Subst.add_type *)

(*s: function Subst.add_module *)
let add_module id p s =
  { modules = Ident.add id p s.modules;
    (* boilerplate, with record *)
    types = s.types;
    modtypes = s.modtypes }
(*e: function Subst.add_module *)

(*s: function Subst.add_modtype *)
let add_modtype id ty s =
  { modtypes = Ident.add id ty s.modtypes;
    (* boilerplate, with record *)
    modules = s.modules;
    types = s.types;
  }
(*e: function Subst.add_modtype *)

(*s: function Subst.module_path *)
let rec module_path s = function
    Pident id as p ->
      begin try Ident.find_same id s.modules with Not_found -> p end
  | Pdot(p, n, pos) ->
      Pdot(module_path s p, n, pos)
(*e: function Subst.module_path *)

(*s: function Subst.type_path *)
let type_path s = function
    Pident id as p ->
      begin try Ident.find_same id s.types with Not_found -> p end
  | Pdot(p, n, pos) ->
      Pdot(module_path s p, n, pos)
(*e: function Subst.type_path *)

(*s: function Subst.type_expr *)
let rec type_expr s = function
    Tvar{tvar_link = None} as ty -> ty
  | Tvar{tvar_link = Some ty} -> type_expr s ty
  | Tconstr(p, []) -> Tconstr(type_path s p, [])
  | Tconstr(p, tl) -> Tconstr(type_path s p, List.map (type_expr s) tl)
  (* boilerplate mapper *)
  | Tarrow(t1, t2) -> Tarrow(type_expr s t1, type_expr s t2)
  | Ttuple tl -> Ttuple(List.map (type_expr s) tl)
(*e: function Subst.type_expr *)

(*s: function Subst.value_description *)
let value_description s descr =
  { val_type = type_expr s descr.val_type;
    val_prim = descr.val_prim }
(*e: function Subst.value_description *)

(*s: function Subst.type_declaration *)
let type_declaration s decl =
  { type_params = decl.type_params;
    type_arity = decl.type_arity;
    type_kind =
      begin match decl.type_kind with 
       (* boilerplate mapper *)
        Type_abstract -> Type_abstract
      | Type_variant cstrs ->
          Type_variant(cstrs |> List.map (fun (n, args) -> 
                          (n, List.map (type_expr s) args)))
      | Type_record lbls ->
          Type_record(lbls |> List.map (fun (n, mut, arg) -> 
                          (n, mut, type_expr s arg)))
      end;
    type_manifest =
      begin match decl.type_manifest with
        None -> None
      | Some ty -> Some(type_expr s ty)
      end
  }
(*e: function Subst.type_declaration *)

(*s: function Subst.exception_declaration *)
let exception_declaration s tyl =
  List.map (type_expr s) tyl
(*e: function Subst.exception_declaration *)

(*s: function Subst.modtype *)
let rec modtype s = function
    Tmty_ident p as mty ->
      begin match p with
        Pident id ->
          begin try Ident.find_same id s.modtypes with Not_found -> mty end
      | Pdot(p, n, pos) ->
          Tmty_ident(Pdot(module_path s p, n, pos))
      end
  | Tmty_signature sg ->
      Tmty_signature(signature s sg)

and signature s sg = List.map (signature_item s) sg

and signature_item s = function
    Tsig_value(id, d) -> Tsig_value(id, value_description s d)
  | Tsig_type(id, d) -> Tsig_type(id, type_declaration s d)
  | Tsig_exception(id, d) -> Tsig_exception(id, exception_declaration s d)
  | Tsig_module(id, mty) -> Tsig_module(id, modtype s mty)
(*e: function Subst.modtype *)

(*e: ./typing/subst.ml *)
