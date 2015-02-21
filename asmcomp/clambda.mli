(*s: asmcomp/clambda.mli *)
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

(* $Id: clambda.mli,v 1.9 1997/02/16 17:20:08 xleroy Exp $ *)

(* A variant of the "lambda" code with direct / indirect calls explicit
   and closures explicit too *)

open Asttypes
open Lambda

(*s: type Clambda.function_label *)
type function_label = string
(*e: type Clambda.function_label *)

(*s: type Clambda.ulambda *)
type ulambda =
    Uvar of Ident.t
  | Uconst of structured_constant
  | Udirect_apply of function_label * ulambda list
  | Ugeneric_apply of ulambda * ulambda list
  | Uclosure of (function_label * int * Ident.t list * ulambda) list
              * ulambda list
  | Uoffset of ulambda * int
  | Ulet of Ident.t * ulambda * ulambda
  | Uletrec of (Ident.t * ulambda) list * ulambda
  | Uprim of primitive * ulambda list
  | Uswitch of ulambda * ulambda_switch
  | Ustaticfail
  | Ucatch of ulambda * ulambda
  | Utrywith of ulambda * Ident.t * ulambda
  | Uifthenelse of ulambda * ulambda * ulambda
  | Usequence of ulambda * ulambda
  | Uwhile of ulambda * ulambda
  | Ufor of Ident.t * ulambda * ulambda * direction_flag * ulambda
  | Uassign of Ident.t * ulambda
  | Usend of ulambda * ulambda * ulambda list
(*e: type Clambda.ulambda *)

(*s: type Clambda.ulambda_switch *)
and ulambda_switch =
  { us_index_consts: int array;
    us_cases_consts: ulambda array;
    us_index_blocks: int array;
    us_cases_blocks: ulambda array;
    us_checked: bool }
(*e: type Clambda.ulambda_switch *)

(*s: type Clambda.function_description *)
(* Description of known functions *)

type function_description =
  { fun_label: function_label;          (* Label of direct entry point *)
    fun_arity: int;                     (* Number of arguments *)
    mutable fun_closed: bool;           (* True if environment not used *)
    mutable fun_inline: (Ident.t list * ulambda) option }
(*e: type Clambda.function_description *)

(*s: type Clambda.value_approximation *)
(* Approximation of values *)

type value_approximation =
    Value_closure of function_description * value_approximation
  | Value_tuple of value_approximation array
  | Value_unknown
(*e: type Clambda.value_approximation *)
(*e: asmcomp/clambda.mli *)
