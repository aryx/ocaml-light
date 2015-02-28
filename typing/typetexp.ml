(*s: ./typing/typetexp.ml *)
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

(* Typechecking of type expressions for the core language *)

open Parsetree
open Types
open Ctype

(*s: exception Typetexp.Already_bound *)
exception Already_bound
(*e: exception Typetexp.Already_bound *)

(*s: type Typetexp.error *)
type error =
    Unbound_type_variable of string
  | Unbound_type_constructor of Longident.t
  | Type_arity_mismatch of Longident.t * int * int
(*e: type Typetexp.error *)

(*s: exception Typetexp.Error *)
exception Error of Location.t * error
(*e: exception Typetexp.Error *)

(*s: constant Typetexp.type_variables *)
(* Translation of type expressions *)

let type_variables = ref (Tbl.empty : (string, Types.type_expr) Tbl.t)
(*e: constant Typetexp.type_variables *)

(*s: function Typetexp.reset_type_variables *)
let reset_type_variables () =
  type_variables := Tbl.empty
(*e: function Typetexp.reset_type_variables *)

(*s: function Typetexp.enter_type_variable *)
let enter_type_variable name =
  try
    Tbl.find name !type_variables; 
    raise Already_bound
  with Not_found ->
    let v = new_global_var() in
    type_variables := Tbl.add name v !type_variables;
    v
(*e: function Typetexp.enter_type_variable *)

(*s: function Typetexp.transl_simple_type *)
let rec transl_simple_type env fixed styp =
  match styp.ptyp_desc with
    Ptyp_var name ->
      begin try
        Tbl.find name !type_variables
      with Not_found ->
        if fixed then
          raise(Error(styp.ptyp_loc, Unbound_type_variable name))
        else begin
          let v = new_global_var() in
          type_variables := Tbl.add name v !type_variables;
          v
        end
      end
  | Ptyp_constr(lid, stl) ->
      let (path, decl) =
        try
          Env.lookup_type lid env
        with Not_found ->
          raise(Error(styp.ptyp_loc, Unbound_type_constructor lid)) in
      if List.length stl <> decl.type_arity 
      then
        raise(Error(styp.ptyp_loc, Type_arity_mismatch(lid, decl.type_arity,
                                                           List.length stl)));
      Tconstr(path, List.map (transl_simple_type env fixed) stl)
  (* boilerplate mapper *)
  | Ptyp_arrow(st1, st2) ->
      Tarrow(transl_simple_type env fixed st1,
             transl_simple_type env fixed st2)
  | Ptyp_tuple stl ->
      Ttuple(List.map (transl_simple_type env fixed) stl)
(*e: function Typetexp.transl_simple_type *)

(*s: function Typetexp.transl_type_scheme *)
let transl_type_scheme env styp =
  reset_type_variables();
  begin_def();
  let typ = transl_simple_type env false styp in
  end_def();
  generalize typ;
  typ
(*e: function Typetexp.transl_type_scheme *)

(* Error report *)

open Format
open Printtyp

(*s: function Typetexp.report_error *)
let report_error = function
    Unbound_type_variable name ->
      print_string "Unbound type parameter "; print_string name
  | Unbound_type_constructor lid ->
      print_string "Unbound type constructor "; longident lid
  | Type_arity_mismatch(lid, expected, provided) ->
      open_hovbox 0;
      print_string "The type constructor "; longident lid;
      print_space(); print_string "expects "; print_int expected;
      print_string " argument(s),"; print_space();
      print_string "but is here applied to "; print_int provided;
      print_string " argument(s)";
      close_box()
(*e: function Typetexp.report_error *)
(*e: ./typing/typetexp.ml *)
