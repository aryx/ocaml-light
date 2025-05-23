(*s: typing/datarepr.ml *)
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

(* Compute constructor and label descriptions from type declarations,
   determining their representation. *)

open Asttypes
open Types

(*s: function [[Datarepr.constructor_descrs]] *)
let constructor_descrs ty_res cstrs =
  let num_consts = ref 0 in
  let num_nonconsts = ref 0 in
  cstrs |> List.iter (function 
    | (_name, []) -> incr num_consts
    | (_name, _)  -> incr num_nonconsts
  );
  let rec describe_constructors idx_const idx_nonconst = function
      [] -> []
    | (name, ty_args) :: rem ->
        let (tag, descr_rem) =
          match ty_args with
            [] -> (Cstr_constant idx_const,
                   describe_constructors (idx_const+1) idx_nonconst rem)
          | _  -> (Cstr_block idx_nonconst,
                   describe_constructors idx_const (idx_nonconst+1) rem) 
        in
        let cstr =
          { cstr_res = ty_res;
            cstr_args = ty_args;
            cstr_arity = List.length ty_args;
            cstr_tag = tag;
            cstr_consts = !num_consts;
            cstr_nonconsts = !num_nonconsts } 
        in
        (name, cstr) :: descr_rem in
  describe_constructors 0 0 cstrs
(*e: function [[Datarepr.constructor_descrs]] *)

(*s: function [[Datarepr.exception_descr]] *)
let exception_descr path_exc decl =
  { cstr_res = Predef.type_exn;
    cstr_args = decl;
    cstr_arity = List.length decl;
    cstr_tag = Cstr_exception path_exc;
    cstr_consts = -1;
    cstr_nonconsts = -1 }
(*e: function [[Datarepr.exception_descr]] *)

(*s: constant [[Datarepr.dummy_label]] *)
let dummy_label =
  { lbl_res = Ttuple []; lbl_arg = Ttuple []; lbl_mut = Immutable;
    lbl_pos = (-1); lbl_all = [||]; lbl_repres = Record_regular }
(*e: constant [[Datarepr.dummy_label]] *)

(*s: function [[Datarepr.is_float]] *)
(* Cannot call ctype.repres here *)

let rec is_float = function
    Tvar {tvar_link = Some ty} -> is_float ty
  | Tconstr(p, _) -> Path.same p Predef.path_float
  | _ -> false
(*e: function [[Datarepr.is_float]] *)

(*s: function [[Datarepr.label_descrs]] *)
let label_descrs ty_res lbls =
  let all_labels = Array.create (List.length lbls) dummy_label in
  let repres =
    if List.for_all (fun (_name, _flag, ty) -> is_float ty) lbls
    then Record_float
    else Record_regular in
  let rec describe_labels num = function
      [] -> []
    | (name, mut_flag, ty_arg) :: rest ->
        let lbl =
          { lbl_res = ty_res;
            lbl_arg = ty_arg;
            lbl_mut = mut_flag;
            lbl_pos = num;
            lbl_all = all_labels;
            lbl_repres = repres } in
        all_labels.(num) <- lbl;
        (name, lbl) :: describe_labels (num+1) rest in
  describe_labels 0 lbls
(*e: function [[Datarepr.label_descrs]] *)
(*e: typing/datarepr.ml *)
