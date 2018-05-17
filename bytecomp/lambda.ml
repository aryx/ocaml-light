(*s: ./bytecomp/lambda.ml *)
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

open Misc
open Path
open Asttypes

(*s: type [[Lambda.primitive]] *)
type primitive =
    Pidentity

  (* Globals *)
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t

  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag

  | Pfield of int
  | Psetfield of int * bool

  (*s: [[Lambda.primitive]] operations on heap blocks other cases *)
  | Pfloatfield of int
  | Psetfloatfield of int
  (*e: [[Lambda.primitive]] operations on heap blocks other cases *)

  (* External call *)
  | Pccall of Primitive.description

  (* Exceptions *)
  | Praise

  (* Boolean operations *)
  | Psequand | Psequor | Pnot

  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint

  | Pintcomp of comparison

  | Poffsetint of int
  | Poffsetref of int

  (* Float operations *)
  (*s: [[Lambda.primitive]] float operations cases *)
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of comparison
  (*e: [[Lambda.primitive]] float operations cases *)
  (* String operations *)
  (*s: [[Lambda.primitive]] string operations cases *)
  | Pstringlength | Pstringrefu | Pstringsetu | Pstringrefs | Pstringsets
  (*e: [[Lambda.primitive]] string operations cases *)
  (* Array operations *)
  (*s: [[Lambda.primitive]] array operations cases *)
  | Pmakearray of array_kind
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind
  (*e: [[Lambda.primitive]] array operations cases *)

  (* Bitvect operations *)
  | Pbittest
(*e: type [[Lambda.primitive]] *)

(*s: type [[Lambda.comparison]] *)
and comparison =
    Ceq | Cneq | Clt | Cgt | Cle | Cge
(*e: type [[Lambda.comparison]] *)

(*s: type [[Lambda.array_kind]] *)
and array_kind =
  | Pgenarray 
  | Paddrarray 
  | Pintarray 
  | Pfloatarray
(*e: type [[Lambda.array_kind]] *)

(*s: type [[Lambda.structured_constant]] *)
type structured_constant =
    Const_base of Asttypes.constant
  | Const_pointer of int
  | Const_block of int * structured_constant list
  (*s: [[Lambda.structured_constant]] other cases *)
  | Const_float_array of string list
  (*e: [[Lambda.structured_constant]] other cases *)
(*e: type [[Lambda.structured_constant]] *)

(*s: type [[Lambda.function_kind]] *)
type function_kind = Curried | Tupled
(*e: type [[Lambda.function_kind]] *)

(*s: type [[Lambda.let_kind]] *)
type let_kind = Strict | Alias | StrictOpt
(*e: type [[Lambda.let_kind]] *)

(*s: type [[Lambda.shared_code]] *)
type shared_code = (int * int) list     (* stack size -> code label *)
(*e: type [[Lambda.shared_code]] *)

(*s: type [[Lambda.lambda]] *)
type lambda =
    Lvar of Ident.t
  | Lconst of structured_constant

  | Lapply of lambda * lambda list
  | Lfunction of function_kind * Ident.t list * lambda

  | Llet of let_kind * Ident.t * lambda * lambda
  | Lletrec of (Ident.t * lambda) list * lambda

  | Lprim of primitive * lambda list

  | Lswitch of lambda * lambda_switch
  | Lstaticfail

  | Lcatch of lambda * lambda
  | Ltrywith of lambda * Ident.t * lambda

  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda
  | Lassign of Ident.t * lambda

  (*s: [[Lambda.lambda]] other cases *)
  | Levent of lambda * lambda_event
  (*e: [[Lambda.lambda]] other cases *)
(*e: type [[Lambda.lambda]] *)

(*s: type [[Lambda.lambda_switch]] *)
and lambda_switch =
  { sw_numconsts: int;                  (* Number of integer cases *)
    sw_consts: (int * lambda) list;     (* Integer cases *)
    sw_numblocks: int;                  (* Number of tag block cases *)
    sw_blocks: (int * lambda) list;     (* Tag block cases *)
    sw_checked: bool }                  (* True if bound checks needed *)
(*e: type [[Lambda.lambda_switch]] *)

(*s: type [[Lambda.lambda_event]] *)
and lambda_event =
  { lev_loc: int;
    lev_kind: lambda_event_kind;
    lev_repr: int ref option;
    lev_env: unit; (*Env.summary*) }
(*e: type [[Lambda.lambda_event]] *)

(*s: type [[Lambda.lambda_event_kind]] *)
and lambda_event_kind =
    Lev_before
  | Lev_after of Types.type_expr
  | Lev_function
(*e: type [[Lambda.lambda_event_kind]] *)

(*s: constant [[Lambda.const_unit]] *)
let const_unit = Const_pointer 0
(*e: constant [[Lambda.const_unit]] *)

(*s: constant [[Lambda.lambda_unit]] *)
let lambda_unit = Lconst const_unit
(*e: constant [[Lambda.lambda_unit]] *)

(*s: function [[Lambda.name_lambda]] *)
let name_lambda arg fn =
  match arg with
    Lvar id -> fn id
  | _ -> let id = Ident.create "let" in Llet(Strict, id, arg, fn id)
(*e: function [[Lambda.name_lambda]] *)

module IdentSet = Set

(*s: function [[Lambda.free_variables]] *)
let free_variables l =
  let fv = ref IdentSet.empty in
  let rec freevars = function
    Lvar id ->
      fv := IdentSet.add id !fv
  | Lconst sc -> ()
  | Lapply(fn, args) ->
      freevars fn; List.iter freevars args
  | Lfunction(kind, params, body) ->
      freevars body;
      List.iter (fun param -> fv := IdentSet.remove param !fv) params
  | Llet(str, id, arg, body) ->
      freevars arg; freevars body; fv := IdentSet.remove id !fv
  | Lletrec(decl, body) ->
      freevars body;
      List.iter (fun (id, exp) -> freevars exp) decl;
      List.iter (fun (id, exp) -> fv := IdentSet.remove id !fv) decl
  | Lprim(p, args) ->
      List.iter freevars args
  | Lswitch(arg, sw) ->
      freevars arg; 
      List.iter (fun (key, case) -> freevars case) sw.sw_consts;
      List.iter (fun (key, case) -> freevars case) sw.sw_blocks
  | Lstaticfail -> ()
  | Lcatch(e1, e2) ->
      freevars e1; freevars e2
  | Ltrywith(e1, exn, e2) ->
      freevars e1; freevars e2; fv := IdentSet.remove exn !fv
  | Lifthenelse(e1, e2, e3) ->
      freevars e1; freevars e2; freevars e3
  | Lsequence(e1, e2) ->
      freevars e1; freevars e2
  | Lwhile(e1, e2) ->
      freevars e1; freevars e2
  | Lfor(v, e1, e2, dir, e3) -> 
      freevars e1; freevars e2; freevars e3; fv := IdentSet.remove v !fv
  | Lassign(id, e) ->
      fv := IdentSet.add id !fv; freevars e
  | Levent (lam, evt) ->
      freevars lam
  in freevars l; !fv
(*e: function [[Lambda.free_variables]] *)

(*s: constant [[Lambda.is_guarded]] *)
(* Check if an action has a "when" guard *)

let rec is_guarded = function
    Lifthenelse(cond, body, Lstaticfail) -> true
  | Llet(str, id, lam, body) -> is_guarded body
  | Levent(lam, ev) -> is_guarded lam
  | _ -> false
(*e: constant [[Lambda.is_guarded]] *)

(*s: constant [[Lambda.transl_path]] *)
let rec transl_path = function
    Pident id ->
      if Ident.global id 
      then Lprim(Pgetglobal id, []) 
      else Lvar id
  | Pdot(p, s, pos) ->
      Lprim(Pfield pos, [transl_path p])
(*e: constant [[Lambda.transl_path]] *)
(*e: ./bytecomp/lambda.ml *)
