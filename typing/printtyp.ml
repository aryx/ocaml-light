(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: printtyp.ml,v 1.38 1997/12/09 09:12:51 xleroy Exp $ *)

(* Printing functions *)

open Misc
open Ctype
open Format
open Longident
open Path
open Asttypes
open Types
open Btype

(* Print a long identifier *)

let rec longident = function
    Lident s -> print_string s
  | Ldot(p, s) -> longident p; print_string "."; print_string s

(* Print an identifier *)

let ident id =
  print_string(Ident.name id)

(* Print a path *)

let ident_pervasive = Ident.create_persistent "Pervasives"

let rec path = function
    Pident id ->
      ident id
  | Pdot(Pident id, s, pos) when Ident.same id ident_pervasive ->
      print_string s
  | Pdot(p, s, pos) ->
      path p; print_string "."; print_string s

(* Print a type expression *)

let names = ref ([] : (type_expr * string) list)
let name_counter = ref 0

let reset_names () = names := []; name_counter := 0

let new_name () =
  let name =
    if !name_counter < 26
    then String.make 1 (Char.chr(97 + !name_counter)) 
    else String.make 1 (Char.chr(97 + !name_counter mod 26)) ^
           string_of_int(!name_counter / 26)
  in
    incr name_counter;
    name

let name_of_type t =
  try List.assq t !names with Not_found ->
    let name = new_name () in
    names := (t, name) :: !names;
    name

let rec list_removeq a =
  function
    [] ->
      []
  | (b, _) as e::l ->
      if a == b then l else e::list_removeq a l

let remove_name_of_type t =
  names := list_removeq t !names

let visited_objects = ref ([] : type_expr list)
let aliased = ref ([] : type_expr list)

let rec mark_loops_rec visited ty =
  let ty = repr ty in
  if List.memq ty visited then begin
    if not (List.memq ty !aliased) then
      aliased := ty :: !aliased
  end else
    let visited = ty :: visited in
    match ty.desc with
      Tvar                -> ()
    | Tarrow(ty1, ty2)    ->
        mark_loops_rec visited ty1; mark_loops_rec visited ty2
    | Ttuple tyl          -> List.iter (mark_loops_rec visited) tyl
    | Tconstr(_, tyl, _)  ->
        List.iter (mark_loops_rec visited) tyl
    | Tnil                -> ()
    | Tlink _             -> fatal_error "Printtyp.mark_loops_rec (2)"

let mark_loops ty = mark_loops_rec [] ty

let reset_loop_marks () =
  visited_objects := []; aliased := []

let reset () =
  reset_names (); reset_loop_marks ()

let rec typexp sch prio0 ty =
  let ty = repr ty in
  try
    List.assq ty !names;
    if (ty.desc = Tvar) && sch && (ty.level <> generic_level)
    then print_string "'_"
    else print_string "'";
    print_string (name_of_type ty)
  with Not_found ->
    let alias = List.memq ty !aliased in
    if alias then begin
      name_of_type ty;
      if prio0 >= 1 then begin open_box 1; print_string "(" end
      else open_box 0
    end;
    let prio = if alias then 0 else prio0 in
    begin match ty.desc with
      Tvar ->
        if (not sch) or ty.level = generic_level
        then print_string "'"
        else print_string "'_";
        print_string(name_of_type ty)
    | Tarrow(ty1, ty2) ->
        if prio >= 2 then begin open_box 1; print_string "(" end
                     else open_box 0;
        typexp sch 2 ty1;
        print_string " ->"; print_space();
        typexp sch 1 ty2;
        if prio >= 2 then print_string ")";
        close_box()
    | Ttuple tyl ->
        if prio >= 3 then begin open_box 1; print_string "(" end
                     else open_box 0;
        typlist sch 3 " *" tyl;
        if prio >= 3 then print_string ")";
        close_box()
    | Tconstr(p, tyl, abbrev) ->
        open_box 0;
        begin match tyl with
          [] -> ()
        | [ty1] ->
            typexp sch 3 ty1; print_space()
        | tyl ->
            open_box 1; print_string "("; typlist sch 0 "," tyl;
            print_string ")"; close_box(); print_space()
        end;
        path p;
        close_box()
(*
| Tfield _ -> typobject sch ty ty (ref None)
| Tnil -> typobject sch ty ty (ref None)
*)
    | _ ->
        fatal_error "Printtyp.typexp"
    end;
    if alias then begin
      print_string " as ";
      print_string "'";
      print_string (name_of_type ty);
      (* if not (opened_object ty) then *)
      remove_name_of_type ty;
      if prio0 >= 1 then print_string ")";
      close_box()
    end
(* ; print_string "["; print_int ty.level; print_string "]" *)

and typlist sch prio sep = function
    [] -> ()
  | [ty] -> typexp sch prio ty
  | ty::tyl ->
      typexp sch prio ty; print_string sep; print_space();
      typlist sch prio sep tyl

and typfields sch rest =
  function
    [] ->
      begin match rest.desc with
        Tvar -> if sch & rest.level <> generic_level then
                  print_string "_";
                print_string ".."
      | Tnil -> ()
      | _    -> fatal_error "typfields (1)"
      end
  | [(s, t)] ->
      print_string s;
      print_string " : ";
      typexp sch 0 t;
      begin match rest.desc with
        Tvar -> print_string ";"; print_space ()
      | Tnil -> ()
      | _    -> fatal_error "typfields (2)"
      end;
      typfields sch rest []
  | (s, t)::l ->
      print_string s;
      print_string " : ";
      typexp sch 0 t;
      print_string ";"; print_space ();
      typfields sch rest l

let type_expr ty =
  typexp false 0 ty

and type_sch ty =
  typexp true 0 ty

and type_scheme ty =
  reset(); mark_loops ty; typexp true 0 ty

(* Print one type declaration *)

let constrain ty =
  let ty' = unalias ty in
  if ty != ty' then begin
    print_space ();
    open_box 2;
    print_string "constraint ";
    type_sch ty;
    print_string " =";
    print_space();
    type_sch ty';
    close_box()
  end

let rec type_declaration id decl =
  reset();

  let params = List.map repr decl.type_params in

  aliased := params @ !aliased;
  List.iter mark_loops params;
  List.iter (fun x -> name_of_type x; ()) params;
  begin match decl.type_manifest with
    None    -> ()
  | Some ty -> mark_loops ty
  end;
  begin match decl.type_kind with
    Type_abstract -> ()
  | Type_variant [] -> ()
  | Type_variant cstrs ->
      List.iter (fun (_, args) -> List.iter mark_loops args) cstrs
  | Type_record (lbl1 :: lbls as l) ->
      List.iter (fun (_, _, ty) -> mark_loops ty) l
  | _ -> assert false
  end;

  open_hvbox 2;
  print_string "type ";
  type_expr {desc = Tconstr(Pident id, params, ref Mnil);
             level = generic_level};
  begin match decl.type_manifest with
    None -> ()
  | Some ty ->
      print_string " ="; print_space(); type_expr ty
  end;
  begin match decl.type_kind with
    Type_abstract -> ()
  | Type_variant [] -> ()
      (* A fatal error actually, except when printing type exn... *)
  | Type_variant cstrs ->
      print_string " =";
      List.iter
        (fun cstr -> print_space(); print_string "| "; constructor cstr)
        cstrs
  | Type_record (lbl1 :: lbls as l) ->
      print_string " ="; print_space();
      print_string "{ "; label lbl1;
      List.iter
        (fun lbl -> print_string ";"; print_break 1 2; label lbl)
        lbls;
      print_string " }"
  | _ -> assert false
  end;
  List.iter constrain params;
  close_box()

and constructor (name, args) =
  print_string name;
  match args with
    [] -> ()
  | _  -> print_string " of ";
          open_box 2; typlist false 3 " *" args; close_box()

and label (name, mut, arg) =
  begin match mut with
      Immutable -> ()
    | Mutable -> print_string "mutable "
  end;
  print_string name;
  print_string ": ";
  type_expr arg

(* Print an exception declaration *)

let exception_declaration id decl =
  print_string "exception "; constructor (Ident.name id, decl)

(* Print a value declaration *)

let value_description id decl =
  open_box 2;
  print_string (if decl.val_kind = Val_reg then "val " else "external ");
  ident id; print_string " :"; print_space();
  type_scheme decl.val_type;
  begin match decl.val_kind with
    Val_prim p ->
      print_space(); print_string "= "; Primitive.print_description p
  | _ -> ()
  end;
  close_box()


(* Print a module type *)

let rec modtype = function
    Tmty_ident p ->
      path p
  | Tmty_signature sg ->
      open_hvbox 2;
      print_string "sig"; signature_body true sg; 
      print_break 1 (-2); print_string "end";
      close_box()

and signature_body spc = function
    [] -> ()
  | item :: rem ->
      if spc then print_space();
      let cont =
        match item with
          Tsig_value(id, decl) ->
            value_description id decl; rem
        | Tsig_type(id, decl)  ->
            type_declaration id decl; rem
        | Tsig_exception(id, decl)  ->
            exception_declaration id decl; rem
        | Tsig_module(id, mty)  ->
            open_box 2; print_string "module "; ident id; print_string " :";
            print_space(); modtype mty; close_box(); rem
        | Tsig_modtype(id, decl)  ->
            modtype_declaration id decl; rem
      in signature_body true cont

and modtype_declaration id decl =
  open_box 2; print_string "module type "; ident id;
  begin match decl with
    Tmodtype_abstract -> ()
  | Tmodtype_manifest mty ->
      print_string " ="; print_space(); modtype mty
  end;
  close_box()

(* Print a signature body (used when compiling a .mli and printing results
   in interactive use). *)

let signature sg =
  open_vbox 0;
  signature_body false sg;
  close_box()

(* Print an unification error *)

let type_expansion t t' =
  if t == t' then
    type_expr t
  else begin
    open_box 2;
    type_expr t;
    print_space (); print_string "="; print_space ();
    type_expr t';
    close_box ()
  end

let rec trace fst txt =
  function
    (t1, t1')::(t2, t2')::rem ->
      if not fst then
        print_cut ();
      open_box 0;
      print_string "Type"; print_break 1 2;
      type_expansion t1 t1'; print_space ();
      txt (); print_break 1 2;
      type_expansion t2 t2';
      close_box ();
      trace false txt rem
  | _ ->
      ()

let unification_error tr txt1 txt2 =
  reset ();
  List.iter
    (function (t, t') -> mark_loops t; if t != t' then mark_loops t')
    tr;
  open_box 0;
  let (t1, t1') = List.hd tr in
  let (t2, t2') = List.hd (List.tl tr) in
  txt1 (); print_break 1 2;
  type_expansion t1 t1'; print_space();
  txt2 (); print_break 1 2;
  type_expansion t2 t2';
  close_box();
  trace false (fun _ -> print_string "is not compatible with type")
        (List.tl (List.tl tr))
