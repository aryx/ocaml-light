(*s: ./bytecomp/lambda.mli *)
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

(* The "lambda" intermediate code *)

open Asttypes

(*s: type Lambda.primitive *)
type primitive =
    Pidentity

  (* Globals *)
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t

  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag
  | Pfield of int
  | Psetfield of int * bool
  | Pfloatfield of int
  | Psetfloatfield of int

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
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of comparison

  (* String operations *)
  | Pstringlength | Pstringrefu | Pstringsetu | Pstringrefs | Pstringsets

  (* Array operations *)
  | Pmakearray of array_kind
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind

  (* Bitvect operations *)
  | Pbittest
(*e: type Lambda.primitive *)

(*s: type Lambda.comparison *)
and comparison =
    Ceq | Cneq | Clt | Cgt | Cle | Cge
(*e: type Lambda.comparison *)

(*s: type Lambda.array_kind *)
and array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray
(*e: type Lambda.array_kind *)

(*s: type Lambda.structured_constant *)
type structured_constant =
    Const_base of constant
  | Const_pointer of int
  | Const_block of int * structured_constant list
  | Const_float_array of string list
(*e: type Lambda.structured_constant *)

(*s: type Lambda.function_kind *)
type function_kind = Curried | Tupled
(*e: type Lambda.function_kind *)

(*s: type Lambda.let_kind *)
type let_kind = Strict | Alias | StrictOpt
(*e: type Lambda.let_kind *)

(*s: type Lambda.shared_code *)
type shared_code = (int * int) list     (* stack size -> code label *)
(*e: type Lambda.shared_code *)

(*s: type Lambda.lambda *)
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
  | Levent of lambda * lambda_event
(*e: type Lambda.lambda *)

(*s: type Lambda.lambda_switch *)
and lambda_switch =
  { sw_numconsts: int;                  (* Number of integer cases *)
    sw_consts: (int * lambda) list;     (* Integer cases *)
    sw_numblocks: int;                  (* Number of tag block cases *)
    sw_blocks: (int * lambda) list;     (* Tag block cases *)
    sw_checked: bool }                  (* True if bound checks needed *)
(*e: type Lambda.lambda_switch *)

(*s: type Lambda.lambda_event *)
and lambda_event =
  { lev_loc: int;
    lev_kind: lambda_event_kind;
    lev_repr: int ref option;
    lev_env: unit; (*Env.summary*) }
(*e: type Lambda.lambda_event *)

(*s: type Lambda.lambda_event_kind *)
and lambda_event_kind =
    Lev_before
  | Lev_after of Types.type_expr
  | Lev_function
(*e: type Lambda.lambda_event_kind *)

(*s: signature Lambda.const_unit *)
val const_unit: structured_constant
(*e: signature Lambda.const_unit *)
(*s: signature Lambda.lambda_unit *)
val lambda_unit: lambda
(*e: signature Lambda.lambda_unit *)
(*s: signature Lambda.name_lambda *)
val name_lambda: lambda -> (Ident.t -> lambda) -> lambda
(*e: signature Lambda.name_lambda *)
(*s: signature Lambda.is_guarded *)
val is_guarded: lambda -> bool
(*e: signature Lambda.is_guarded *)

(*s: signature Lambda.free_variables *)
val free_variables: lambda -> Ident.t Set.t
(*e: signature Lambda.free_variables *)

(*s: signature Lambda.transl_path *)
val transl_path: Path.t -> lambda
(*e: signature Lambda.transl_path *)
(*e: ./bytecomp/lambda.mli *)
