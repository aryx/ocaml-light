(*s: asmcomp/asmlink.ml *)
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

(* Link a set of .cmx/.o files and produce an executable *)

open Sys
open Misc
open Config
open Compilenv

(*s: type [[Asmlink.error]]([[(asmcomp/asmlink.ml)]]) *)
type error =
    File_not_found of string
  | Not_an_object_file of string
  | Missing_implementations of string list
  | Inconsistent_interface of string * string * string
  | Inconsistent_implementation of string * string * string
  | Assembler_error of string
  | Linking_error
(*e: type [[Asmlink.error]]([[(asmcomp/asmlink.ml)]]) *)

(*s: exception [[Asmlink.Error]]([[(asmcomp/asmlink.ml)]]) *)
exception Error of error
(*e: exception [[Asmlink.Error]]([[(asmcomp/asmlink.ml)]]) *)

(*s: constant [[Asmlink.crc_interfaces]] *)
(* Consistency check between interfaces and implementations *)

let crc_interfaces =
(*e: constant [[Asmlink.crc_interfaces]] *)
      (Hashtbl.create 17 : (string, string * Digest.t) Hashtbl.t)
(*s: constant [[Asmlink.crc_implementations]] *)
let crc_implementations =
(*e: constant [[Asmlink.crc_implementations]] *)
      (Hashtbl.create 17 : (string, string * Digest.t) Hashtbl.t)

(*s: function [[Asmlink.check_consistency]] *)
let check_consistency file_name unit crc =
  List.iter
    (fun (name, crc) ->
      if name = unit.ui_name then begin
        Hashtbl.add crc_interfaces name (file_name, crc)
      end else begin
        try
          let (auth_name, auth_crc) = Hashtbl.find crc_interfaces name in
          if crc <> auth_crc then
            raise(Error(Inconsistent_interface(name, file_name, auth_name)))
        with Not_found ->
          (* Can only happen for unit for which only a .cmi file was used,
             but no .cmo is provided *)
          Hashtbl.add crc_interfaces name (file_name, crc)
      end)
    unit.ui_imports_cmi;
  List.iter
    (fun (name, crc) ->
      if crc <> cmx_not_found_crc then begin
      try
        let (auth_name, auth_crc) = Hashtbl.find crc_implementations name in
        if crc <> auth_crc then
          raise(Error(Inconsistent_implementation(name, file_name, auth_name)))
      with Not_found ->
        Hashtbl.add crc_implementations name (file_name, crc)
      end)
    unit.ui_imports_cmx;
  Hashtbl.add crc_implementations unit.ui_name (file_name, crc)
(*e: function [[Asmlink.check_consistency]] *)

(* First pass: determine which units are needed *)

module StringSet = Set

(*s: constant [[Asmlink.missing_globals]] *)
let missing_globals = ref StringSet.empty
(*e: constant [[Asmlink.missing_globals]] *)

(*s: function [[Asmlink.is_required]] *)
let is_required name =
  StringSet.mem name !missing_globals
(*e: function [[Asmlink.is_required]] *)

(*s: function [[Asmlink.add_required]] *)
let add_required (name, crc) =
  missing_globals := StringSet.add name !missing_globals
(*e: function [[Asmlink.add_required]] *)

(*s: function [[Asmlink.remove_required]] *)
let remove_required name =
  missing_globals := StringSet.remove name !missing_globals
(*e: function [[Asmlink.remove_required]] *)

(*s: function [[Asmlink.scan_file]] *)
let scan_file obj_name tolink =
  let file_name =
    try
      find_in_path !load_path obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  if Filename.check_suffix file_name ".cmx" then begin
    (* This is a .cmx file. It must be linked in any case.
       Read the infos to see which modules it requires. *)
    let (info, crc) = Compilenv.read_unit_info file_name in
    check_consistency file_name info crc;
    remove_required info.ui_name;
    List.iter add_required info.ui_imports_cmx;
    info :: tolink
  end
  else if Filename.check_suffix file_name ".cmxa" then begin
    (* This is an archive file. Each unit contained in it will be linked
       in only if needed. *)
    let ic = open_in(*_bin*) file_name in
    let buffer = Bytes.create (String.length cmxa_magic_number) in
    really_input ic buffer 0 (String.length cmxa_magic_number);
    if Bytes.to_string buffer <> cmxa_magic_number then
      raise(Error(Not_an_object_file file_name));
    let info_crc_list = (input_value ic : (unit_infos * Digest.t) list) in
    close_in ic;
    List.fold_right
      (fun (info, crc) reqd ->
        if info.ui_force_link
        || !Clflags.link_everything
        || is_required info.ui_name then begin
          check_consistency file_name info crc;
          remove_required info.ui_name;
          List.iter add_required info.ui_imports_cmx;
          info :: reqd
        end else
          reqd)
    info_crc_list tolink
  end
  else raise(Error(Not_an_object_file file_name))
(*e: function [[Asmlink.scan_file]] *)

(* Second pass: generate the startup file and link it with everything else *)

module IntSet = Set

(*s: function [[Asmlink.make_startup_file]] *)
let make_startup_file filename info_list =
  let oc = open_out filename in
  Emitaux.output_channel := oc;
  Location.input_name := "startup"; (* set the name of the "current" input *)
  Compilenv.reset "startup"; (* set the name of the "current" compunit *)
  Emit.begin_assembly();
  let name_list = List.map (fun ui -> ui.ui_name) info_list in
  Asmgen.compile_phrase(Cmmgen.entry_point name_list);
  let apply_functions = ref (IntSet.add 2 (IntSet.add 3 IntSet.empty)) in
  (* The callback functions always reference caml_apply[23] *)
  let curry_functions =
    ref IntSet.empty in
  List.iter
    (fun info ->
      List.iter
        (fun n -> apply_functions := IntSet.add n !apply_functions)
        info.ui_apply_fun;
      List.iter
        (fun n -> curry_functions := IntSet.add n !curry_functions)
        info.ui_curry_fun)
    info_list;
  IntSet.iter
    (fun n -> Asmgen.compile_phrase(Cmmgen.apply_function n))
    !apply_functions;
  IntSet.iter
    (fun n -> List.iter Asmgen.compile_phrase (Cmmgen.curry_function n))
    !curry_functions;
  Array.iter
    (fun name -> Asmgen.compile_phrase(Cmmgen.predef_exception name))
    Runtimedef.builtin_exceptions;
  Asmgen.compile_phrase(Cmmgen.global_table name_list);
  Asmgen.compile_phrase(Cmmgen.data_segment_table name_list);
  Asmgen.compile_phrase(Cmmgen.code_segment_table name_list);
  Asmgen.compile_phrase
    (Cmmgen.frame_table("startup" :: "system" :: name_list));
  Emit.end_assembly();
  close_out oc
(*e: function [[Asmlink.make_startup_file]] *)

(*s: function [[Asmlink.call_linker]] *)
let call_linker file_list startup_file =
  let libname = "libasmrun" ^ ext_lib in
  let runtime_lib =
    try
      find_in_path !load_path libname
    with Not_found ->
      raise(Error(File_not_found libname)) in
  let cmd =
    match Config.system with
    | _ ->
        if not !Clflags.output_c_object then
          Printf.sprintf "%s -o %s -I%s %s %s %s -L%s %s %s %s"
            Config.native_c_compiler
            !Clflags.exec_name
            Config.standard_library
            (String.concat " " (List.rev !Clflags.ccopts))
            startup_file
            (String.concat " " (List.rev file_list))
            Config.standard_library
            (String.concat " " (List.rev !Clflags.ccobjs))
            runtime_lib
            Config.c_libraries
        else
          Printf.sprintf "%s -o %s %s %s"
            Config.native_partial_linker
            !Clflags.object_name
            startup_file
            (String.concat " " (List.rev file_list))
  in if Ccomp.command cmd <> 0 then raise(Error Linking_error)
(*e: function [[Asmlink.call_linker]] *)

(*s: function [[Asmlink.object_file_name]] *)
let object_file_name name =
  let file_name =
    try
      find_in_path !load_path name
    with Not_found ->
      fatal_error "Asmlink.object_file_name: not found" in
  if Filename.check_suffix file_name ".cmx" then
    Filename.chop_suffix file_name ".cmx" ^ ext_obj
  else if Filename.check_suffix file_name ".cmxa" then
    Filename.chop_suffix file_name ".cmxa" ^ ext_lib
  else
    fatal_error "Asmlink.object_file_name: bad ext"
(*e: function [[Asmlink.object_file_name]] *)

(*s: function [[Asmlink.link]] *)
(* Main entry point *)

let link objfiles =
  let objfiles = "stdlib.cmxa" :: (objfiles @ ["std_exit.cmx"]) in
  let units_tolink = List.fold_right scan_file objfiles [] in
  Array.iter remove_required Runtimedef.builtin_exceptions;
  if not (StringSet.is_empty !missing_globals) then
    raise(Error(Missing_implementations(StringSet.elements !missing_globals)));
  let startup = Filename.temp_file "camlstartup" ext_asm in
  make_startup_file startup units_tolink;
  let startup_obj = Filename.temp_file "camlstartup" ext_obj in
  if Proc.assemble_file startup startup_obj <> 0 then
    raise(Error(Assembler_error startup));
  try
    call_linker (List.map object_file_name objfiles) startup_obj;
    if not !Clflags.keep_startup_file then remove_file startup;
    remove_file startup_obj
  with x ->
    remove_file startup_obj;
    raise x
(*e: function [[Asmlink.link]] *)

(* Error report *)

open Format

(*s: function [[Asmlink.report_error]] *)
let report_error = function
    File_not_found name ->
      print_string "Cannot find file "; print_string name
  | Not_an_object_file name ->
      print_string "The file "; print_string name;
      print_string " is not a compilation unit description"
  | Missing_implementations l ->
      open_box 0;
      print_string
        "No implementation(s) provided for the following module(s):";
      List.iter (fun s -> print_space(); print_string s) l;
      close_box()
  | Inconsistent_interface(intf, file1, file2) ->
      open_hvbox 0;
      print_string "Files "; print_string file1; print_string " and ";
      print_string file2; print_space();
      print_string "make inconsistent assumptions over interface ";
      print_string intf;
      close_box()
  | Inconsistent_implementation(intf, file1, file2) ->
      open_hvbox 0;
      print_string "Files "; print_string file1; print_string " and ";
      print_string file2; print_space();
      print_string "make inconsistent assumptions over implementation ";
      print_string intf;
      close_box()
  | Assembler_error file ->
      print_string "Error while assembling "; print_string file
  | Linking_error ->
      print_string "Error during linking"
(*e: function [[Asmlink.report_error]] *)
(*e: asmcomp/asmlink.ml *)
