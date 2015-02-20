(*s: ./typing/subst.ml *)
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

(* Substitutions *)

open Misc
open Path
open Types
open Btype

(*s: type Subst.t *)
type t = 
  { types: (Ident.t, Path.t) Tbl.t;
    modules: (Ident.t, Path.t) Tbl.t;
    modtypes: (Ident.t, module_type) Tbl.t }
(*e: type Subst.t *)

(*s: constant Subst.identity *)
let identity =
  { types = Tbl.empty; modules = Tbl.empty; modtypes = Tbl.empty }
(*e: constant Subst.identity *)

(*s: function Subst.add_type *)
let add_type id p s =
  { types = Tbl.add id p s.types;
    modules = s.modules;
    modtypes = s.modtypes }
(*e: function Subst.add_type *)

(*s: function Subst.add_module *)
let add_module id p s =
  { types = s.types;
    modules = Tbl.add id p s.modules;
    modtypes = s.modtypes }
(*e: function Subst.add_module *)

(*s: function Subst.add_modtype *)
let add_modtype id ty s =
  { types = s.types;
    modules = s.modules;
    modtypes = Tbl.add id ty s.modtypes }
(*e: function Subst.add_modtype *)

(*s: function Subst.remove_type *)
let remove_type id s =
  { types = Tbl.remove id s.types;
    modules = s.modules;
    modtypes = s.modtypes }
(*e: function Subst.remove_type *)

(*s: function Subst.remove_module *)
let remove_module id s =
  { types = s.types;
    modules = Tbl.remove id s.modules;
    modtypes = s.modtypes }
(*e: function Subst.remove_module *)

(*s: function Subst.remove_modtype *)
let remove_modtype id s =
  { types = s.types;
    modules = s.modules;
    modtypes = Tbl.remove id s.modtypes }
(*e: function Subst.remove_modtype *)

(*s: function Subst.module_path *)
let rec module_path s = function
    Pident id as p ->
      begin try Tbl.find id s.modules with Not_found -> p end
  | Pdot(p, n, pos) ->
      Pdot(module_path s p, n, pos)
(*e: function Subst.module_path *)

(*s: function Subst.type_path *)
let type_path s = function
    Pident id as p ->
      begin try Tbl.find id s.types with Not_found -> p end
  | Pdot(p, n, pos) ->
      Pdot(module_path s p, n, pos)
(*e: function Subst.type_path *)

(*s: function Subst.typexp *)
(* Similar to [Ctype.nondep_type_rec]. *)
let rec typexp s ty =
  let ty = repr ty in
  if (ty.desc = Tvar) || (ty.level < lowest_level) then
    ty
  else begin
    let desc = ty.desc in
    save_desc ty desc;
    let ty' = newmarkedgenvar () in     (* Stub *)
    ty.desc <- Tlink ty';
    ty'.desc <-
      begin match desc with
        Tvar | Tlink _ ->
          fatal_error "Subst.typexp"
      | Tarrow(t1, t2) ->
          Tarrow(typexp s t1, typexp s t2)
      | Ttuple tl ->
          Ttuple(List.map (typexp s) tl)
      | Tconstr(p, tl, abbrev) ->
          Tconstr(type_path s p, List.map (typexp s) tl, ref Mnil)
      | Tnil ->
          Tnil
      end;
    ty'
  end
(*e: function Subst.typexp *)

(*s: function Subst.type_expr *)
(*
   Always make a copy of the type. If this is not done, type levels
   might not be correct.
*)
let type_expr s ty =
  let ty' = typexp s ty in
  cleanup_types ();
  unmark_type ty';
  ty'
(*e: function Subst.type_expr *)

(*s: function Subst.type_declaration *)
let type_declaration s decl =
  let decl =
    { type_params = List.map (typexp s) decl.type_params;
      type_arity = decl.type_arity;
      type_kind =
        begin match decl.type_kind with
          Type_abstract -> Type_abstract
        | Type_variant cstrs ->
            Type_variant(
              List.map (fun (n, args) -> (n, List.map (typexp s) args))
                       cstrs)
        | Type_record lbls ->
            Type_record(
              List.map (fun (n, mut, arg) -> (n, mut, typexp s arg))
                       lbls)
        end;
      type_manifest =
        begin match decl.type_manifest with
          None -> None
        | Some ty -> Some(typexp s ty)
        end
    }
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
(*e: function Subst.type_declaration *)


(*s: function Subst.value_description *)
let value_description s descr =
  { val_type = type_expr s descr.val_type;
    val_kind = descr.val_kind }
(*e: function Subst.value_description *)

(*s: function Subst.exception_declaration *)
let exception_declaration s tyl =
  List.map (type_expr s) tyl
(*e: function Subst.exception_declaration *)

let rec modtype s = function
    Tmty_ident p as mty ->
      begin match p with
        Pident id ->
          begin try Tbl.find id s.modtypes with Not_found -> mty end
      | Pdot(p, n, pos) ->
          Tmty_ident(Pdot(module_path s p, n, pos))
      end
  | Tmty_signature sg ->
      Tmty_signature(signature s sg)

and signature s = function
    [] -> []
  | Tsig_value(id, d) :: sg ->
      Tsig_value(id, value_description s d) :: signature s sg
  | Tsig_type(id, d) :: sg ->
      Tsig_type(id, type_declaration s d) :: signature (remove_type id s) sg
  | Tsig_exception(id, d) :: sg ->
      Tsig_exception(id, exception_declaration s d) :: signature s sg
  | Tsig_module(id, mty) :: sg ->
      Tsig_module(id, modtype s mty) :: signature (remove_module id s) sg
  | Tsig_modtype(id, d) :: sg ->
      Tsig_modtype(id, modtype_declaration s d) ::
      signature (remove_modtype id s) sg

and modtype_declaration s = function
    Tmodtype_abstract -> Tmodtype_abstract
  | Tmodtype_manifest mty -> Tmodtype_manifest(modtype s mty)
(*e: ./typing/subst.ml *)
