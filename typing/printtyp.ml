(*s: ./typing/printtyp.ml *)
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

(* Printing functions *)

open Format
open Longident
open Path
open Asttypes
open Types
open Typedtree


(*s: function Printtyp.longident *)
(* Print a long identifier *)

let rec longident = function
    Lident s -> print_string s
  | Ldot(p, s) -> longident p; print_string "."; print_string s
(*e: function Printtyp.longident *)

(*s: function Printtyp.ident *)
(* Print an identifier *)

let ident id =
  print_string(Ident.name id)
(*e: function Printtyp.ident *)

(*s: constant Printtyp.ident_pervasive *)
(* Print a path *)

let ident_pervasive = Ident.create_persistent "Pervasives"
(*e: constant Printtyp.ident_pervasive *)

(*s: function Printtyp.path *)
let rec path = function
    Pident id ->
      ident id
  | Pdot(Pident id, s, pos) when Ident.same id ident_pervasive ->
      print_string s
  | Pdot(p, s, pos) ->
      path p; print_string "."; print_string s
(*e: function Printtyp.path *)

(*s: constant Printtyp.var_names *)
(* Print a type expression *)

let var_names = ref ([] : (type_expr * string) list)
(*e: constant Printtyp.var_names *)
(*s: constant Printtyp.var_counter *)
let var_counter = ref 0
(*e: constant Printtyp.var_counter *)

(*s: function Printtyp.reset_var_names *)
let reset_var_names () = 
  var_names := []; 
  var_counter := 0
(*e: function Printtyp.reset_var_names *)

(*s: function Printtyp.name_of_var *)
let name_of_var v =
  try
    List.assq v !var_names
  with Not_found ->
    let name = 
      if !var_counter < 26
      then String.make 1 (Char.chr(97 + !var_counter)) 
      else String.make 1 (Char.chr(97 + !var_counter mod 26)) ^
           string_of_int(!var_counter / 26) in
    var_names := (v, name) :: !var_names;
    incr var_counter;
    name
(*e: function Printtyp.name_of_var *)

(*s: function Printtyp.typeexp *)
let rec typexp sch prio = function
    Tvar {tvar_link = Some ty} ->
      typexp sch prio ty
  | Tvar {tvar_link = None; tvar_level = lvl} as v ->
      if (not sch) or lvl = -1 (* generic *)
      then print_string "'"
      else print_string "'_";
      print_string(name_of_var v)
  | Tarrow(ty1, ty2) ->
      if prio >= 1 then begin open_hovbox 1; print_string "(" end
                   else open_hovbox 0;
      typexp sch 1 ty1;
      print_string " ->"; print_space();
      typexp sch 0 ty2;
      if prio >= 1 then print_string ")";
      close_box()
  | Ttuple tyl ->
      if prio >= 2 then begin open_hovbox 1; print_string "(" end
                   else open_hovbox 0;
      typlist sch 2 " *" tyl;
      if prio >= 2 then print_string ")";
      close_box()
  | Tconstr(p, tyl) ->
      open_hovbox 0;
      begin match tyl with
        [] -> ()
      | [ty1] ->
          typexp sch 2 ty1; print_space()
      | tyl ->
          open_hovbox 1; print_string "("; typlist sch 0 "," tyl;
          print_string ")"; close_box(); print_space()
      end;
      path p;
      close_box()

and typlist sch prio sep = function
    [] -> ()
  | [ty] -> typexp sch prio ty
  | ty::tyl ->
      typexp sch prio ty; print_string sep; print_space();
      typlist sch prio sep tyl

(*e: function Printtyp.typeexp *)
(*s: function Printtyp.type_expr *)
let type_expr ty = 
  typexp false 0 ty
(*e: function Printtyp.type_expr *)

(*s: function Printtyp.type_scheme *)
and type_scheme ty = 
  reset_var_names(); 
  typexp true 0 ty
(*e: function Printtyp.type_scheme *)

(* Print one type declaration *)

(*s: function Printtyp.type_declaration *)
let rec type_declaration id decl =
  reset_var_names();
  open_hvbox 2;
  print_string "type ";
  type_expr (Tconstr(Pident id, decl.type_params));
  begin match decl.type_manifest with
    None -> ()
  | Some ty -> print_string " ="; print_space(); type_expr ty
  end;
  begin match decl.type_kind with
    Type_abstract -> ()
  | Type_variant [] -> ()
      (* A fatal error actually, except when printing type exn... *)
  | Type_variant (cstr1 :: cstrs) ->
      print_string " ="; print_break 1 2;
      constructor cstr1;
      List.iter
        (fun cstr -> print_space(); print_string "| "; constructor cstr)
        cstrs
  | Type_record (lbl1 :: lbls) ->
      print_string " ="; print_space();
      print_string "{ "; label lbl1;
      List.iter
        (fun lbl -> print_string ";"; print_break 1 2; label lbl)
        lbls;
      print_string " }"
  end;
  close_box()

and constructor (name, args) =
  print_string name;
  match args with
    [] -> ()
  | _  -> print_string " of ";
          open_hovbox 2; typlist false 2 " *" args; close_box()

and label (name, mut, arg) =
  begin match mut with
      Immutable -> ()
    | Mutable -> print_string "mutable "
  end;
  print_string name;
  print_string ": ";
  type_expr arg
(*e: function Printtyp.type_declaration *)

(*s: function Printtyp.exception_declaration *)
(* Print an exception declaration *)

let exception_declaration id decl =
  print_string "exception "; constructor (Ident.name id, decl)
(*e: function Printtyp.exception_declaration *)

(*s: function Printtyp.value_description *)
(* Print a value declaration *)

let value_description id decl =
  open_hovbox 2;
  print_string "val "; ident id; print_string " :"; print_space();
  type_scheme decl.val_type;
  begin match decl.val_prim with
    None -> ()
  | Some p -> print_space(); print_string "= "; Primitive.print_description p
  end;
  close_box()
(*e: function Printtyp.value_description *)

(* Print a module type *)

(*s: function Printtyp.modtype *)

let rec modtype = function
    Tmty_ident p ->
      path p
  | Tmty_signature [] ->
      print_string "sig end"
  | Tmty_signature(item :: rem) ->
      open_hvbox 2;
      print_string "sig"; print_space(); 
      signature_item item;
      List.iter
        (fun item -> print_space(); signature_item item)
      rem;
      print_break 1 (-2); print_string "end";
      close_box()

and signature_item = function
    Tsig_value(id, decl) ->
      value_description id decl
  | Tsig_type(id, decl) ->
      type_declaration id decl
  | Tsig_exception(id, decl) ->
      exception_declaration id decl
  | Tsig_module(id, mty) ->
      open_hovbox 2; print_string "module "; ident id; print_string " :";
      print_space(); modtype mty; close_box()

(*e: function Printtyp.modtype *)

(*s: function Printtyp.signature *)
(* Print a signature body (used when compiling a .mli and printing results
   in interactive use). *)

let signature sg =
  open_vbox 0;
  List.iter (fun item -> signature_item item; print_space()) sg;
  close_box()
(*e: function Printtyp.signature *)
(*e: ./typing/printtyp.ml *)
