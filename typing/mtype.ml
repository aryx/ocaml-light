(*s: ./typing/mtype.ml *)
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

(* Operations on module types *)

open Path
open Types


(*s: function Mtype.scrape *)
let rec scrape env mty =
  match mty with
    Tmty_ident p ->
      failwith "TODO: find_modtype_Expansion"
(*
      begin try
        Env.find_modtype_expansion p env
      with Not_found ->
        mty
      end
*)
  | _ -> mty
(*e: function Mtype.scrape *)

let rec strengthen env mty p =
  match scrape env mty with
    Tmty_signature sg ->
      Tmty_signature(strengthen_sig env sg p)
  | mty ->
      mty

and strengthen_sig env sg p =
  match sg with
    [] -> []
  | (Tsig_value(id, desc) as sigelt) :: rem ->
      sigelt :: strengthen_sig env rem p
  | Tsig_type(id, decl) :: rem ->
      let newdecl =
        match decl.type_manifest with
          None ->
            { type_params = decl.type_params;
              type_arity = decl.type_arity;
              type_kind = decl.type_kind;
              type_manifest = Some(Ctype.newgenty(
                                   Tconstr(Pdot(p, Ident.name id, nopos),
                                           decl.type_params,
                                           ref Mnil))) }
        | _ -> decl in
      Tsig_type(id, newdecl) :: strengthen_sig env rem p
  | (Tsig_exception(id, d) as sigelt) :: rem ->
      sigelt :: strengthen_sig env rem p
  | Tsig_module(id, mty) :: rem ->
      Tsig_module(id, strengthen env mty (Pdot(p, Ident.name id, nopos))) ::
      strengthen_sig (Env.add_module id mty env) rem p
      (* Need to add the module in case it defines manifest module types *)
(*e: ./typing/mtype.ml *)
