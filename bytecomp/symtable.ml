(*s: ./bytecomp/symtable.ml *)
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

(* To assign numbers to globals and primitives *)

open Misc
open Asttypes
open Lambda
open Emitcode


(*s: type Symtable.error *)
(* Error report *)

type error =
    Undefined_global of string
  | Unavailable_primitive of string
(*e: type Symtable.error *)

(*s: exception Symtable.Error *)
exception Error of error
(*e: exception Symtable.Error *)

(*s: type Symtable.numtable *)
(* Tables for numbering objects *)

type 'a numtable =
  { num_cnt: int;               (* The next number *)
    num_tbl: ('a, int) Tbl.t } (* The table of already numbered objects *)
(*e: type Symtable.numtable *)

(*s: constant Symtable.empty_numtable *)
let empty_numtable = { num_cnt = 0; num_tbl = Tbl.empty }
(*e: constant Symtable.empty_numtable *)

(*s: function Symtable.find_numtable *)
let find_numtable nt key =
  Tbl.find key nt.num_tbl
(*e: function Symtable.find_numtable *)

(*s: function Symtable.enter_numtable *)
let enter_numtable nt key =
  let n = !nt.num_cnt in
  nt := { num_cnt = n + 1; num_tbl = Tbl.add key n !nt.num_tbl };
  n
(*e: function Symtable.enter_numtable *)

(*s: function Symtable.incr_numtable *)
let incr_numtable nt =
  let n = !nt.num_cnt in
  nt := { num_cnt = n + 1; num_tbl = !nt.num_tbl };
  n
(*e: function Symtable.incr_numtable *)

(* Global variables *)

let global_table = ref(empty_numtable : Ident.t numtable)
and literal_table = ref([] : (int * structured_constant) list)

(*s: function Symtable.slot_for_getglobal *)
let slot_for_getglobal id =
  try
    find_numtable !global_table id
  with Not_found ->
    raise(Error(Undefined_global(Ident.name id)))
(*e: function Symtable.slot_for_getglobal *)

(*s: function Symtable.slot_for_setglobal *)
let slot_for_setglobal id =
  enter_numtable global_table id
(*e: function Symtable.slot_for_setglobal *)

(*s: function Symtable.slot_for_literal *)
let slot_for_literal cst =
  let n = incr_numtable global_table in
  literal_table := (n, cst) :: !literal_table;
  n
(*e: function Symtable.slot_for_literal *)

(*s: constant Symtable.c_prim_table *)
(* The C primitives *)

let c_prim_table = ref(empty_numtable : string numtable)
(*e: constant Symtable.c_prim_table *)

(*s: function Symtable.num_of_prim *)
let num_of_prim name =
  try
    find_numtable !c_prim_table name
  with Not_found ->
    if !Clflags.custom_runtime
    then enter_numtable c_prim_table name
    else raise(Error(Unavailable_primitive name))
(*e: function Symtable.num_of_prim *)

(*s: function Symtable.require_primitive *)
let require_primitive name =
  if name.[0] <> '%' then begin num_of_prim name; () end
(*e: function Symtable.require_primitive *)

(*s: function Symtable.all_primitives *)
let all_primitives () =
  let prim = Array.create !c_prim_table.num_cnt "" in
  Tbl.iter (fun name number -> prim.(number) <- name) !c_prim_table.num_tbl;
  prim
(*e: function Symtable.all_primitives *)

(*s: function Symtable.output_primitive_names *)
let output_primitive_names outchan =
  let prim = all_primitives() in
  for i = 0 to Array.length prim - 1 do
    output_string outchan prim.(i); output_char outchan '\000'
  done
(*e: function Symtable.output_primitive_names *)

open Printf

(*s: function Symtable.output_primitive_table *)
let output_primitive_table outchan =
  let prim = all_primitives() in
  for i = 0 to Array.length prim - 1 do
    fprintf outchan "extern long %s();\n" prim.(i)
  done;
  fprintf outchan "typedef long (*primitive)();\n";
  fprintf outchan "primitive cprim[] = {\n";
  for i = 0 to Array.length prim - 1 do
    fprintf outchan "  %s,\n" prim.(i)
  done;
  fprintf outchan "  (primitive) 0 };\n";
  fprintf outchan "char * names_of_cprim[] = {\n";
  for i = 0 to Array.length prim - 1 do
    fprintf outchan "  \"%s\",\n" prim.(i)
  done;
  fprintf outchan "  (char *) 0 };\n"
(*e: function Symtable.output_primitive_table *)

(*s: function Symtable.init *)
(* Initialization for batch linking *)

let init () =
  (* Enter the predefined exceptions *)
  Array.iter 
    (fun name -> 
      let id =
        try List.assoc name Predef.builtin_values
        with Not_found -> fatal_error "Symtable.init" in
      let c = slot_for_setglobal id in
      let cst = Const_block(0, [Const_base(Const_string name)]) in
      literal_table := (c, cst) :: !literal_table)
    Runtimedef.builtin_exceptions;
  (* Enter the known C primitives *)
  Array.iter (fun x -> enter_numtable c_prim_table x; ())
             Runtimedef.builtin_primitives
(*e: function Symtable.init *)

(* Relocate a block of object bytecode *)

(*s: function Symtable.patch_int *)
(* Must use the unsafe String.set here because the block may be
   a "fake" string as returned by Meta.static_alloc. *)

let patch_int buff pos n =
  String.unsafe_set buff pos (Char.unsafe_chr n);
  String.unsafe_set buff (pos + 1) (Char.unsafe_chr (n asr 8));
  String.unsafe_set buff (pos + 2) (Char.unsafe_chr (n asr 16));
  String.unsafe_set buff (pos + 3) (Char.unsafe_chr (n asr 24))
(*e: function Symtable.patch_int *)

(*s: function Symtable.patch_object *)
let patch_object buff patchlist = 
  List.iter
    (function
        (Reloc_literal sc, pos) ->
          patch_int buff pos (slot_for_literal sc)
      | (Reloc_getglobal id, pos) ->
          patch_int buff pos (slot_for_getglobal id)
      | (Reloc_setglobal id, pos) ->
          patch_int buff pos (slot_for_setglobal id)
      | (Reloc_primitive name, pos) ->
          patch_int buff pos (num_of_prim name))
    patchlist
(*e: function Symtable.patch_object *)

(*s: constant Symtable.transl_const *)
(* Translate structured constants *)

let rec transl_const = function
    Const_base(Const_int i) -> Obj.repr i
  | Const_base(Const_char c) -> Obj.repr c
  | Const_base(Const_string s) -> Obj.repr s
  | Const_base(Const_float f) -> Obj.repr(float_of_string f)
  | Const_pointer i -> Obj.repr i
  | Const_block(tag, fields) ->
      let block = Obj.new_block tag (List.length fields) in
      let pos = ref 0 in
      List.iter
        (fun c -> Obj.set_field block !pos (transl_const c); incr pos)
        fields;
      block
  | Const_float_array fields ->
      transl_const
        (Const_block(0, List.map (fun f -> Const_base(Const_float f)) fields))
(*e: constant Symtable.transl_const *)

(*s: function Symtable.initial_global_table *)
(* Build the initial table of globals *)

let initial_global_table () =
  let glob = Array.create !global_table.num_cnt (Obj.repr 0) in
  List.iter
    (fun (slot, cst) -> glob.(slot) <- transl_const cst)
    !literal_table;
  literal_table := [];
  glob
(*e: function Symtable.initial_global_table *)

(*s: function Symtable.output_global_map *)
(* Save the table of globals *)

let output_global_map oc =
  output_value oc !global_table
(*e: function Symtable.output_global_map *)

(* Functions for toplevel use *)

(*s: function Symtable.update_global_table *)
(* Update the in-core table of globals *)

let update_global_table () =
  let ng = !global_table.num_cnt in
  if ng > Array.length(Meta.global_data()) then Meta.realloc_global_data ng;
  let glob = Meta.global_data() in
  List.iter
    (fun (slot, cst) -> glob.(slot) <- transl_const cst)
    !literal_table;
  literal_table := []
(*e: function Symtable.update_global_table *)

(*s: function Symtable.init_toplevel *)
(* Initialize the linker for toplevel use *)

let init_toplevel () =
  (* Read back the known global symbols from the executable file *)
  let ic = open_in_bin Sys.argv.(0) in
  let pos_trailer =
    in_channel_length ic - 20 - String.length Config.exec_magic_number in
  seek_in ic pos_trailer;
  let code_size = input_binary_int ic in
  let prim_size = input_binary_int ic in
  let data_size = input_binary_int ic in
  let symbol_size = input_binary_int ic in
  let debug_size = input_binary_int ic in
  seek_in ic (pos_trailer - debug_size - symbol_size);
  global_table := (input_value ic : Ident.t numtable);
  close_in ic;
  (* Enter the known C primitives *)
  Array.iter (fun x -> enter_numtable c_prim_table x; ())
             (Meta.available_primitives())
(*e: function Symtable.init_toplevel *)

(* Find the value of a global identifier *)

(*s: function Symtable.get_global_position *)
(* @Scheck: used by the debugger *)
let get_global_position id = slot_for_getglobal id
(*e: function Symtable.get_global_position *)

(*s: function Symtable.get_global_value *)
let get_global_value id =
(*e: function Symtable.get_global_value *)
  (Meta.global_data()).(slot_for_getglobal id)

(*s: type Symtable.global_map *)
(* Save and restore the current state *)

type global_map = Ident.t numtable
(*e: type Symtable.global_map *)

(*s: function Symtable.current_state *)
let current_state () = !global_table
(*e: function Symtable.current_state *)

(*s: function Symtable.restore_state *)
let restore_state st = global_table := st
(*e: function Symtable.restore_state *)

(*s: function Symtable.hide_additions *)
(* @Scheck: used by dynlink *)
let hide_additions st =
  if st.num_cnt > !global_table.num_cnt then
    fatal_error "Symtable.hide_additions";
  global_table :=
    { num_cnt = !global_table.num_cnt;
      num_tbl = st.num_tbl }
(*e: function Symtable.hide_additions *)

(*s: function Symtable.filter_global_map *)
(* "Filter" the global map according to some predicate.
   Used to expunge the global map for the toplevel. *)

let filter_global_map p gmap =
  let newtbl = ref Tbl.empty in
  Tbl.iter
    (fun id num -> if p id then newtbl := Tbl.add id num !newtbl)
    gmap.num_tbl;
  {num_cnt = gmap.num_cnt; num_tbl = !newtbl}
(*e: function Symtable.filter_global_map *)

(* Error report *)

open Format

(*s: constant Symtable.report_error *)
let report_error = function
    Undefined_global s ->
      print_string "Reference to undefined global `"; print_string s;
      print_string "'"
  | Unavailable_primitive s ->
      print_string "The external function `"; print_string s;
      print_string "' is not available"
(*e: constant Symtable.report_error *)
(*e: ./bytecomp/symtable.ml *)
