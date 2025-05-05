(*s: typing/mtype.ml *)
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

(* Operations on module types *)

open Path
open Types


(*s: function [[Mtype.scrape]] *)
let scrape _env mty =
  match mty with
    Tmty_ident _p ->
      failwith "Mtype.scrape:TODO"
(*
      begin try
        match Env.find_modtype p env with
          Tmodtype_abstract -> mty
        | Tmodtype_manifest mty' -> scrape env mty'
      with Not_found ->
        mty
      end
*)
  | _ -> mty
(*e: function [[Mtype.scrape]] *)

let rec strengthen env mty p =
  match scrape env mty with
    Tmty_signature sg ->
      Tmty_signature(strengthen_sig env sg p)
  | mty ->
      mty

and strengthen_sig env sg p =
  match sg with
    [] -> []
  | (Tsig_value(_id, _desc) as sigelt) :: rem ->
      sigelt :: strengthen_sig env rem p
  | Tsig_type(id, decl) :: rem ->
      let newdecl =
        match decl.type_manifest with
          None ->
            { type_params = decl.type_params;
              type_arity = decl.type_arity;
              type_kind = decl.type_kind;
              type_manifest = Some(Tconstr(Pdot(p, Ident.name id, nopos),
                                                decl.type_params)) }
        | _ -> decl in
      Tsig_type(id, newdecl) :: strengthen_sig env rem p
  | (Tsig_exception(_id, _d) as sigelt) :: rem ->
      sigelt :: strengthen_sig env rem p
  | Tsig_module(id, mty) :: rem ->
      Tsig_module(id, strengthen env mty (Pdot(p, Ident.name id, nopos))) ::
      strengthen_sig (Env.add_module id mty env) rem p
      (* Need to add the module in case it defines manifest module types *)

(*s: type [[Mtype.variance]] *)
(* In nondep_supertype, env is only used for the type it assigns to id.
   Hence there is no need to keep env up-to-date by adding the bindings
   traversed. *)

type variance = Co | Contra | Strict
(*e: type [[Mtype.variance]] *)

(*s: function [[Mtype.nondep_supertype]] *)
let nondep_supertype env mid mty =

  let rec nondep_mty var mty =
    match mty with
      Tmty_ident p ->
        if Path.isfree mid p then begin
          failwith "nondep_mty:TODO"
            (*
          match Env.find_modtype p env with
            Tmodtype_abstract -> raise Not_found
          | Tmodtype_manifest mty -> nondep_mty var mty      
            *)
        end else mty
    | Tmty_signature sg ->
        Tmty_signature(nondep_sig var sg)

  and nondep_sig var = function
    [] -> []
  | item :: rem ->
      let rem' = nondep_sig var rem in
      match item with
        Tsig_value(id, d) ->
          Tsig_value(id, {val_type = Ctype.nondep_type env mid d.val_type;
                          val_prim = d.val_prim}) :: rem'
      | Tsig_type(id, d) ->
          Tsig_type(id, nondep_type_decl var d) :: rem'
      | Tsig_exception(id, d) ->
          Tsig_exception(id, List.map (Ctype.nondep_type env mid) d) :: rem'
      | Tsig_module(id, mty) ->
          Tsig_module(id, nondep_mty var mty) :: rem'

  and nondep_type_decl var d =
    {type_params = d.type_params;
     type_arity = d.type_arity;
     type_kind =
       begin try
         match d.type_kind with
           Type_abstract ->
             Type_abstract
         | Type_variant cstrs ->
             Type_variant(List.map
               (fun (c, tl) -> (c, List.map (Ctype.nondep_type env mid) tl))
               cstrs)
         | Type_record lbls ->
             Type_record(List.map
               (fun (c, mut, t) -> (c, mut, Ctype.nondep_type env mid t))
               lbls)
       with Not_found ->
         match var with Co -> Type_abstract | _ -> raise Not_found
       end;
     type_manifest =
       begin try
         match d.type_manifest with
           None -> None
         | Some ty -> Some(Ctype.nondep_type env mid ty)
       with Not_found ->
         match var with Co -> None | _ -> raise Not_found
       end}

  in
    nondep_mty Co mty
(*e: function [[Mtype.nondep_supertype]] *)
(*e: typing/mtype.ml *)
