(*s: ./typing/env.ml *)
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

(* Environment handling *)

open Format
open Config
open Misc
open Asttypes
open Longident
open Path
open Types


(*s: type Env.error *)
(* Error report *)

type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string
(*e: type Env.error *)

(*s: exception Env.Error *)
exception Error of error
(*e: exception Env.Error *)

(*s: type Env.t *)
type t = {
  values: (Path.t * value_description) Ident.tbl;
  constrs: constructor_description Ident.tbl;
  labels: label_description Ident.tbl;
  types: (Path.t * type_declaration) Ident.tbl;
  modules: (Path.t * module_type) Ident.tbl;
  components: (Path.t * module_components) Ident.tbl
}
(*e: type Env.t *)

(*s: type Env.module_components *)
and module_components =
    Structure_comps of structure_components
  | Functor_comps of functor_components
(*e: type Env.module_components *)

(*s: type Env.structure_components *)
and structure_components = {
  mutable comp_values: (string, (value_description * int)) Tbl.t;
  mutable comp_constrs: (string, (constructor_description * int)) Tbl.t;
  mutable comp_labels: (string, (label_description * int)) Tbl.t;
  mutable comp_types: (string, (type_declaration * int)) Tbl.t;
  mutable comp_modules: (string, (module_type * int)) Tbl.t;
  mutable comp_components: (string, (module_components * int)) Tbl.t
}
(*e: type Env.structure_components *)

(*s: type Env.functor_components *)
and functor_components = {
  fcomp_param: Ident.t;
  fcomp_arg: module_type;
  fcomp_res: module_type;
  fcomp_env: t
}
(*e: type Env.functor_components *)

(*s: constant Env.empty *)
let empty = {
  values = Ident.empty; 
  constrs = Ident.empty;
  labels = Ident.empty; 
  types = Ident.empty;
  modules = Ident.empty;
  components = Ident.empty; }
(*e: constant Env.empty *)

(*s: type Env.pers_struct *)
(* Persistent structure descriptions *)

type pers_struct =
  { ps_name: string;
    ps_sig: signature;
    ps_comps: module_components }
(*e: type Env.pers_struct *)

(*s: constant Env.persistent_structures *)
let persistent_structures =
(*e: constant Env.persistent_structures *)
  (Hashtbl.create 17 : (string, pers_struct) Hashtbl.t)

(*s: constant Env.imported_units *)
let imported_units = ref ([] : (string * Digest.t) list)
(*e: constant Env.imported_units *)

(*s: function Env.read_pers_struct *)
let read_pers_struct modname filename =
  let ic = open_in_bin filename in
  try
    let buffer = String.create (String.length cmi_magic_number) in
    really_input ic buffer 0 (String.length cmi_magic_number);
    if buffer <> cmi_magic_number then begin
      close_in ic;
      raise(Error(Not_an_interface filename))
    end;
    let ps = (input_value ic : pers_struct) in
    let crc = Digest.input ic in
    close_in ic;
    if ps.ps_name <> modname then
      raise(Error(Illegal_renaming(ps.ps_name, filename)));
    (ps, crc)
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_interface(filename)))
(*e: function Env.read_pers_struct *)

(*s: function Env.find_pers_struct *)
let find_pers_struct name =
  try
    Hashtbl.find persistent_structures name
  with Not_found ->
    let (ps, crc) =
      read_pers_struct name
        (find_in_path !load_path (String.lowercase name ^ ".cmi")) in
    Hashtbl.add persistent_structures name ps;
    imported_units := (name, crc) :: !imported_units;
    ps
(*e: function Env.find_pers_struct *)

(*s: function Env.reset_cache *)
let reset_cache() =
  Hashtbl.clear persistent_structures;
  imported_units := []
(*e: function Env.reset_cache *)

(*s: constant Env.components_of_functor_appl *)
(*e: constant Env.components_of_functor_appl *)

(*s: constant Env.check_modtype_inclusion *)
let check_modtype_inclusion =
  (* to be filled with includemod.check_modtype_inclusion *)
  ref ((fun env mty1 mty2 -> fatal_error "Env.include_modtypes") :
(*e: constant Env.check_modtype_inclusion *)
       t -> module_type -> module_type -> unit)

(*s: function Env.find_module_descr *)
(* Lookup by identifier *)

let rec find_module_descr path env =
  match path with
    Pident id ->
      begin try
        let (p, desc) = Ident.find_same id env.components
        in desc
      with Not_found ->
        if Ident.persistent id
        then (find_pers_struct (Ident.name id)).ps_comps
        else raise Not_found
      end
  | Pdot(p, s, pos) ->
      begin match find_module_descr p env with
       Structure_comps c ->
      let (descr, pos) = Tbl.find s c.comp_components in
          descr
      | Functor_comps f ->
        raise Not_found
      end
(*e: function Env.find_module_descr *)

(*s: function Env.find *)
let find proj1 proj2 path env =
  match path with
    Pident id ->
      let (p, data) = Ident.find_same id (proj1 env)
      in data
  | Pdot(p, s, pos) ->
      begin match find_module_descr p env with
        Structure_comps c ->
          let (data, pos) = Tbl.find s (proj2 c) in data
      | Functor_comps f ->
          raise Not_found
      end
(*e: function Env.find *)

let find_value = find (fun env -> env.values) (fun sc -> sc.comp_values)
and find_type = find (fun env -> env.types) (fun sc -> sc.comp_types)

(* Lookup by name *)

let rec lookup_module_descr lid env =
  match lid with
    Lident s ->
      begin try
        Ident.find_name s env.components
      with Not_found ->
        (Pident(Ident.create_persistent s), (find_pers_struct s).ps_comps)
      end
  | Ldot(l, s) ->
      let (p, descr) = lookup_module_descr l env in
      begin match descr with
       Structure_comps c ->
      let (descr, pos) = Tbl.find s c.comp_components in
          (Pdot(p, s, pos), descr)
      | Functor_comps f ->
         raise Not_found
      end

and lookup_module lid env =
  match lid with
    Lident s ->
      begin try
        Ident.find_name s env.modules
      with Not_found ->
        (Pident(Ident.create_persistent s), 
         Tmty_signature(find_pers_struct s).ps_sig)
      end
  | Ldot(l, s) ->
      let (p, descr) = lookup_module_descr l env in
      begin match descr with
       Structure_comps c ->
          let (data, pos) = Tbl.find s c.comp_modules in
          (Pdot(p, s, pos), data)
      | Functor_comps f ->
         raise Not_found
      end

(*s: function Env.lookup *)
let lookup proj1 proj2 lid env =
  match lid with
    Lident s ->
      Ident.find_name s (proj1 env)
  | Ldot(l, s) ->
      begin match lookup_module_descr l env with
       (p, Structure_comps c) ->
      let (data, pos) = Tbl.find s (proj2 c) in
          (Pdot(p, s, pos), data)
      | (p, Functor_comps f) ->
         raise Not_found
      end
(*e: function Env.lookup *)

(*s: function Env.lookup_simple *)
let lookup_simple proj1 proj2 lid env =
  match lid with
    Lident s ->
      Ident.find_name s (proj1 env)
  | Ldot(l, s) ->
      begin match lookup_module_descr l env with
       (p, Structure_comps c) ->
      let (data, pos) = Tbl.find s (proj2 c) in
          data
      | (p, Functor_comps f) ->
         raise Not_found
      end
(*e: function Env.lookup_simple *)

let lookup_value =
  lookup (fun env -> env.values) (fun sc -> sc.comp_values)
and lookup_constructor =
  lookup_simple (fun env -> env.constrs) (fun sc -> sc.comp_constrs)
and lookup_label =
  lookup_simple (fun env -> env.labels) (fun sc -> sc.comp_labels)
and lookup_type =
  lookup (fun env -> env.types) (fun sc -> sc.comp_types)

(*s: function Env.scrape_modtype *)
(* Scrape a module type *)

let rec scrape_modtype mty env =
  match mty with
    Tmty_ident path ->
      failwith "Env.scrape_modtype:TODO"
(*
      begin match find_modtype path env with
          Tmodtype_manifest mty -> scrape_modtype mty env
        | Tmodtype_abstract -> mty
      end
*)
  | _ -> mty
(*e: function Env.scrape_modtype *)

(*s: function Env.constructors_of_type *)
(* Compute constructor descriptions *)

let constructors_of_type ty_path decl =
  match decl.type_kind with
    Type_variant cstrs ->
      Datarepr.constructor_descrs (Tconstr(ty_path, decl.type_params)) cstrs
  | _ -> []
(*e: function Env.constructors_of_type *)

(*s: function Env.labels_of_type *)
(* Compute label descriptions *)

let labels_of_type ty_path decl =
  match decl.type_kind with
    Type_record labels ->
      Datarepr.label_descrs (Tconstr(ty_path, decl.type_params)) labels
  | _ -> []
(*e: function Env.labels_of_type *)

(*s: function Env.prefix_idents *)
(* Given a signature and a root path, prefix all idents in the signature
   by the root path and build the corresponding substitution. *)

let rec prefix_idents root pos sub = function
    [] -> ([], sub)
  | Tsig_value(id, decl) :: rem ->
      let p = Pdot(root, Ident.name id, pos) in
      let nextpos = match decl.val_prim with None -> pos+1 | Some _ -> pos in
      let (pl, final_sub) = prefix_idents root nextpos sub rem in
      (p::pl, final_sub)
  | Tsig_type(id, decl) :: rem ->
      let p = Pdot(root, Ident.name id, nopos) in
      let (pl, final_sub) =
        prefix_idents root pos (Subst.add_type id p sub) rem in
      (p::pl, final_sub)
  | Tsig_exception(id, decl) :: rem ->
      let p = Pdot(root, Ident.name id, pos) in
      let (pl, final_sub) = prefix_idents root (pos+1) sub rem in
      (p::pl, final_sub)
  | Tsig_module(id, mty) :: rem ->
      let p = Pdot(root, Ident.name id, pos) in
      let (pl, final_sub) =
        prefix_idents root (pos+1) (Subst.add_module id p sub) rem in
      (p::pl, final_sub)
(*e: function Env.prefix_idents *)

(* Compute structure descriptions *)

let rec components_of_module env sub path mty =
  match scrape_modtype mty env with
    Tmty_signature sg ->
      let c =
        { comp_values = Tbl.empty; comp_constrs = Tbl.empty;
          comp_labels = Tbl.empty; comp_types = Tbl.empty;
          comp_modules = Tbl.empty; 
          comp_components = Tbl.empty } in
      let (pl, sub) = prefix_idents path 0 sub sg in
      let env = ref env in
      let pos = ref 0 in
      List.iter2 (fun item path ->
        match item with
          Tsig_value(id, decl) ->
            let decl' = Subst.value_description sub decl in
            c.comp_values <-
              Tbl.add (Ident.name id) (decl', !pos) c.comp_values;
            begin match decl.val_prim with
              None -> incr pos | Some _ -> ()
            end
        | Tsig_type(id, decl) ->
            let decl' = Subst.type_declaration sub decl in
            c.comp_types <-
              Tbl.add (Ident.name id) (decl', nopos) c.comp_types;
            List.iter
              (fun (name, descr) ->
                c.comp_constrs <- Tbl.add name (descr, nopos) c.comp_constrs)
              (constructors_of_type path decl');
            List.iter
              (fun (name, descr) ->
                c.comp_labels <- Tbl.add name (descr, nopos) c.comp_labels)
              (labels_of_type path decl')
        | Tsig_exception(id, decl) ->
            let decl' = Subst.exception_declaration sub decl in
            let cstr = Datarepr.exception_descr path decl' in
            c.comp_constrs <-
              Tbl.add (Ident.name id) (cstr, !pos) c.comp_constrs;
            incr pos
        | Tsig_module(id, mty) ->
            let mty' = Subst.modtype sub mty in
            c.comp_modules <-
              Tbl.add (Ident.name id) (mty', !pos) c.comp_modules;
            let comps = components_of_module !env sub path mty in
            c.comp_components <-
              Tbl.add (Ident.name id) (comps, !pos) c.comp_components;
            env := store_components id path comps !env;
            incr pos
        )
        sg pl;
    Structure_comps c
  | Tmty_ident p ->
       Structure_comps {
         comp_values = Tbl.empty; comp_constrs = Tbl.empty;
          comp_labels = Tbl.empty; comp_types = Tbl.empty;
          comp_modules = Tbl.empty;
          comp_components = Tbl.empty }

(* Insertion of bindings by identifier + path *)

and store_value id path decl env =
  { values = Ident.add id (path, decl) env.values;
    constrs = env.constrs;
    labels = env.labels;
    types = env.types;
    modules = env.modules;
    components = env.components }

and store_type id path info env =
  { values = env.values;
    constrs =
      List.fold_right
        (fun (name, descr) constrs ->
          Ident.add (Ident.create name) descr constrs)
        (constructors_of_type path info)
        env.constrs;
    labels =
      List.fold_right
        (fun (name, descr) labels ->
          Ident.add (Ident.create name) descr labels)
        (labels_of_type path info)
        env.labels;
    types = Ident.add id (path, info) env.types;
    modules = env.modules;
    components = env.components }

and store_exception id path decl env =
  { values = env.values;
    constrs = Ident.add id (Datarepr.exception_descr path decl) env.constrs;
    labels = env.labels;
    types = env.types;
    modules = env.modules;
    components = env.components }

and store_module id path mty env =
  { values = env.values;
    constrs = env.constrs;
    labels = env.labels;
    types = env.types;
    modules = Ident.add id (path, mty) env.modules;
    components =
      Ident.add id (path, components_of_module env Subst.identity path mty)
                   env.components }

and store_components id path comps env =
  { values = env.values;
    constrs = env.constrs;
    labels = env.labels;
    types = env.types;
    modules = env.modules;
    components = Ident.add id (path, comps) env.components }

(*s: constant Env.funappl_memo *)
(* Memoized function to compute the components of a functor application
   in a path. *)

let funappl_memo =
(*e: constant Env.funappl_memo *)
  (Hashtbl.create 17 : (Path.t, module_components) Hashtbl.t)

(*s: toplevel Env._1 *)
(*e: toplevel Env._1 *)

(* Insertion of bindings by identifier *)

let add_value id desc env =
  store_value id (Pident id) desc env

and add_type id info env =
  store_type id (Pident id) info env

and add_exception id decl env =
  store_exception id (Pident id) decl env

and add_module id mty env =
  store_module id (Pident id) mty env

(*s: function Env.enter *)
(* Insertion of bindings by name *)

let enter store_fun name data env =
  let id = Ident.create name in (id, store_fun id (Pident id) data env)
(*e: function Env.enter *)

let enter_value = enter store_value
and enter_type = enter store_type
and enter_exception = enter store_exception
and enter_module = enter store_module

(*s: function Env.add_signature_component *)
(* Insertion of all components of a signature *)

let add_signature_component env comp =
  match comp with
    Tsig_value(id, decl) -> add_value id decl env
  | Tsig_type(id, decl) -> add_type id decl env
  | Tsig_exception(id, decl) -> add_exception id decl env
  | Tsig_module(id, mty) -> add_module id mty env
(*e: function Env.add_signature_component *)

(*s: function Env.add_signature *)
let add_signature sg env =
  List.fold_left add_signature_component env sg
(*e: function Env.add_signature *)

(*s: function Env.open_signature *)
(* Open a signature path *)

let open_signature root sg env =
  (* First build the paths and substitution *)
  let (pl, sub) = prefix_idents root 0 Subst.identity sg in
  (* Then enter the components in the environment after substitution *)
  List.fold_left2
    (fun env item p ->
      match item with
        Tsig_value(id, decl) ->
          store_value (Ident.hide id) p
                      (Subst.value_description sub decl) env
      | Tsig_type(id, decl) ->
          store_type (Ident.hide id) p
                     (Subst.type_declaration sub decl) env
      | Tsig_exception(id, decl) ->
          store_exception (Ident.hide id) p
                          (Subst.exception_declaration sub decl) env
      | Tsig_module(id, mty) ->
          store_module (Ident.hide id) p (Subst.modtype sub mty) env
    )
    env sg pl
(*e: function Env.open_signature *)

(*s: function Env.open_pers_signature *)
(* Open a signature from a file *)

let open_pers_signature name env =
  let ps = find_pers_struct name in
  open_signature (Pident(Ident.create_persistent name)) ps.ps_sig env
(*e: function Env.open_pers_signature *)

(*s: function Env.read_signature *)
(* Read a signature from a file *)

let read_signature modname filename =
  let (ps, crc) = read_pers_struct modname filename in (ps.ps_sig, crc)
(*e: function Env.read_signature *)

(*s: function Env.save_signature *)
(* Save a signature to a file *)

let save_signature sg modname filename =
  let ps =
    { ps_name = modname;
      ps_sig = sg;
      ps_comps =
        components_of_module empty Subst.identity
            (Pident(Ident.create_persistent modname)) (Tmty_signature sg) } in
  let oc = open_out_bin filename in
  output_string oc cmi_magic_number;
  output_value oc ps;
  flush oc;
  let crc = Digest.file filename in
  Digest.output oc crc;
  close_out oc;
  crc
(*e: function Env.save_signature *)

(*s: constant Env.initial *)
(* Make the initial environment *)

let initial = Predef.build_initial_env add_type add_exception empty
(*e: constant Env.initial *)

(*s: function Env.imported_units *)
(* Return the list of imported interfaces with their CRCs *)

let imported_units() = !imported_units
(*e: function Env.imported_units *)

(*s: function Env.report_error *)
(* Error report *)

let report_error = function
    Not_an_interface filename ->
      print_string filename; print_space();
      print_string "is not a compiled interface."
  | Corrupted_interface filename ->
      print_string "Corrupted compiled interface"; print_space();
      print_string filename
  | Illegal_renaming(modname, filename) ->
      print_string "Wrong file naming:"; print_space();
      print_string filename; print_space();
      print_string "contains the compiled interface for"; print_space();
      print_string modname
(*e: function Env.report_error *)
(*e: ./typing/env.ml *)
