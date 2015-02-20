(*s: ./bytecomp/bytelink.ml *)
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

(* Link a set of .cmo files and produce a bytecode executable. *)

open Sys
open Misc
open Config
open Instruct
open Emitcode

(*s: type Bytelink.error *)
type error =
    File_not_found of string
  | Not_an_object_file of string
  | Symbol_error of string * Symtable.error
  | Inconsistent_import of string * string * string
  | Custom_runtime
  | File_exists of string
(*e: type Bytelink.error *)

(*s: exception Bytelink.Error *)
exception Error of error
(*e: exception Bytelink.Error *)

(*s: type Bytelink.link_action *)
type link_action =
    Link_object of string * compilation_unit
      (* Name of .cmo file and descriptor of the unit *)
  | Link_archive of string * compilation_unit list
      (* Name of .cma file and descriptors of the units to be linked. *)
(*e: type Bytelink.link_action *)

(* First pass: determine which units are needed *)

module IdentSet = Set

(*s: constant Bytelink.missing_globals *)
let missing_globals = ref IdentSet.empty
(*e: constant Bytelink.missing_globals *)

(*s: function Bytelink.is_required *)
let is_required (rel, pos) =
  match rel with
    Reloc_setglobal id ->
      IdentSet.mem id !missing_globals
  | _ -> false
(*e: function Bytelink.is_required *)

(*s: function Bytelink.add_required *)
let add_required (rel, pos) =
  match rel with
    Reloc_getglobal id ->
      missing_globals := IdentSet.add id !missing_globals
  | _ -> ()
(*e: function Bytelink.add_required *)

(*s: function Bytelink.remove_required *)
let remove_required (rel, pos) =
  match rel with
    Reloc_setglobal id ->
      missing_globals := IdentSet.remove id !missing_globals
  | _ -> ()
(*e: function Bytelink.remove_required *)

(*s: function Bytelink.scan_file *)
let scan_file obj_name tolink =
  let file_name =
    try
      find_in_path !load_path obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  let ic = open_in_bin file_name in
  try
    let buffer = String.create (String.length cmo_magic_number) in
    really_input ic buffer 0 (String.length cmo_magic_number);
    if buffer = cmo_magic_number then begin
      (* This is a .cmo file. It must be linked in any case.
         Read the relocation information to see which modules it
         requires. *)
      let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
      seek_in ic compunit_pos;
      let compunit = (input_value ic : compilation_unit) in
      close_in ic;
      List.iter add_required compunit.cu_reloc;
      Link_object(file_name, compunit) :: tolink
    end
    else if buffer = cma_magic_number then begin
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      let pos_toc = input_binary_int ic in    (* Go to table of contents *)
      seek_in ic pos_toc;
      let toc = (input_value ic : compilation_unit list) in
      close_in ic;
      let required =
        List.fold_right
          (fun compunit reqd ->
            if compunit.cu_force_link
            or !Clflags.link_everything
            or List.exists is_required compunit.cu_reloc
            then begin
              List.iter remove_required compunit.cu_reloc;
              List.iter add_required compunit.cu_reloc;
              compunit :: reqd
            end else
              reqd)
          toc [] in
      Link_archive(file_name, required) :: tolink
    end
    else raise(Error(Not_an_object_file file_name))
  with x ->
    close_in ic; raise x
(*e: function Bytelink.scan_file *)

(*s: constant Bytelink.debug_info *)
(* Second pass: link in the required units *)

let debug_info = ref ([] : debug_event list list)
(*e: constant Bytelink.debug_info *)

(*s: constant Bytelink.crc_interfaces *)
(* Consistency check between interfaces *)

let crc_interfaces =
  (Hashtbl.create 17 : (string, string * Digest.t) Hashtbl.t)
(*e: constant Bytelink.crc_interfaces *)

(*s: function Bytelink.check_consistency *)
let check_consistency file_name cu =
  List.iter
    (fun (name, crc) ->
      if name = cu.cu_name then begin
        Hashtbl.add crc_interfaces name (file_name, crc)
      end else begin
        try
          let (auth_name, auth_crc) = Hashtbl.find crc_interfaces name in
          if crc <> auth_crc then
            raise(Error(Inconsistent_import(name, file_name, auth_name)))
        with Not_found ->
          (* Can only happen for unit for which only a .cmi file was used,
             but no .cmo is provided *)
          Hashtbl.add crc_interfaces name (file_name, crc)
      end)
    cu.cu_imports
(*e: function Bytelink.check_consistency *)

(*s: constant Bytelink.debug_info (./bytecomp/bytelink.ml) *)
(* Relocate and record compilation events *)

let debug_info = ref ([] : debug_event list list)
(*e: constant Bytelink.debug_info (./bytecomp/bytelink.ml) *)

(*s: function Bytelink.record_events *)
let record_events orig evl =
  if evl <> [] then begin
    List.iter
      (fun ev ->
         ev.ev_pos <- orig + ev.ev_pos;
         begin match ev.ev_repr with
           Event_parent repr -> repr := ev.ev_pos
         | _                 -> ()
         end)
      evl;
    debug_info := evl :: !debug_info
  end
(*e: function Bytelink.record_events *)

(*s: function Bytelink.link_compunit *)
(* Link in a compilation unit *)

let link_compunit output_fun currpos_fun inchan file_name compunit =
  check_consistency file_name compunit;
  seek_in inchan compunit.cu_pos;
  let code_block = String.create compunit.cu_codesize in
  really_input inchan code_block 0 compunit.cu_codesize;
  Symtable.patch_object code_block compunit.cu_reloc;
  if !Clflags.debug && compunit.cu_debug > 0 then begin
    seek_in inchan compunit.cu_debug;
    record_events (currpos_fun()) (input_value inchan : debug_event list)
  end;
  output_fun code_block;
  if !Clflags.link_everything then
    List.iter Symtable.require_primitive compunit.cu_primitives
(*e: function Bytelink.link_compunit *)

(*s: function Bytelink.link_object *)
(* Link in a .cmo file *)

let link_object output_fun currpos_fun file_name compunit =
  let inchan = open_in_bin file_name in
  try
    link_compunit output_fun currpos_fun inchan file_name compunit;
    close_in inchan
  with
    Symtable.Error msg ->
      close_in inchan; raise(Error(Symbol_error(file_name, msg)))
  | x ->
      close_in inchan; raise x
(*e: function Bytelink.link_object *)

(*s: function Bytelink.link_archive *)
(* Link in a .cma file *)

let link_archive output_fun currpos_fun file_name units_required =
  let inchan = open_in_bin file_name in
  try
    List.iter
      (fun cu ->
         let name = file_name ^ "(" ^ cu.cu_name ^ ")" in
         try
           link_compunit output_fun currpos_fun inchan name cu
         with Symtable.Error msg ->
           raise(Error(Symbol_error(name, msg))))
      units_required;
    close_in inchan
  with x -> close_in inchan; raise x
(*e: function Bytelink.link_archive *)

(*s: function Bytelink.link_file *)
(* Link in a .cmo or .cma file *)

let link_file output_fun currpos_fun = function
    Link_object(file_name, unit) ->
      link_object output_fun currpos_fun file_name unit
  | Link_archive(file_name, units) ->
      link_archive output_fun currpos_fun file_name units
(*e: function Bytelink.link_file *)

(*s: function Bytelink.link_bytecode *)
(* Create a bytecode executable file *)

let link_bytecode objfiles exec_name copy_header =
  let tolink = List.fold_right scan_file objfiles [] in
  if Sys.os_type = "MacOS" then begin
    (* Create it as a text file for bytecode scripts *)
    let c = open_out_gen [Open_wronly; Open_creat] 0o777 exec_name in
    close_out c
  end;
  let outchan = open_out_gen [Open_wronly; Open_trunc; Open_creat; Open_binary]
                             0o777 exec_name in
  try
    (* Copy the header *)
    if copy_header then begin
      try
        let inchan = open_in_bin (find_in_path !load_path "camlheader") in
        copy_file inchan outchan;
        close_in inchan
      with Not_found | Sys_error _ -> ()
    end;
    (* The bytecode *)
    let pos1 = pos_out outchan in
    Symtable.init();
    Hashtbl.clear crc_interfaces;
    let output_fun = output_string outchan
    and currpos_fun () = pos_out outchan - pos1 in
    List.iter (link_file output_fun currpos_fun) tolink;
    (* The final STOP instruction *)
    output_byte outchan Opcodes.opSTOP;
    output_byte outchan 0; output_byte outchan 0; output_byte outchan 0;
    (* The names of all primitives *)
    let pos2 = pos_out outchan in
    Symtable.output_primitive_names outchan;
    (* The table of global data *)
    let pos3 = pos_out outchan in
    output_value outchan (Symtable.initial_global_table());
    (* The map of global identifiers *)
    let pos4 = pos_out outchan in
    Symtable.output_global_map outchan;
    (* Debug info *)
    let pos5 = pos_out outchan in
    if !Clflags.debug then output_value outchan !debug_info;
    (* The trailer *)
    let pos6 = pos_out outchan in
    output_binary_int outchan (pos2 - pos1);
    output_binary_int outchan (pos3 - pos2);
    output_binary_int outchan (pos4 - pos3);
    output_binary_int outchan (pos5 - pos4);
    output_binary_int outchan (pos6 - pos5);
    output_string outchan exec_magic_number;
    close_out outchan
  with x ->
    close_out outchan;
    remove_file exec_name;
    raise x
(*e: function Bytelink.link_bytecode *)

(*s: constant Bytelink.output_code_string_counter *)
(* Output a string as a C array of unsigned ints *)

let output_code_string_counter = ref 0
(*e: constant Bytelink.output_code_string_counter *)

(*s: function Bytelink.output_code_string *)
let output_code_string outchan code =
  let pos = ref 0 in
  let len = String.length code in
  while !pos < len do
    let c1 = Char.code(code.[!pos]) in
    let c2 = Char.code(code.[!pos + 1]) in
    let c3 = Char.code(code.[!pos + 2]) in
    let c4 = Char.code(code.[!pos + 3]) in
    pos := !pos + 4;
    Printf.fprintf outchan "0x%02x%02x%02x%02x, " c4 c3 c2 c1;
    incr output_code_string_counter;
    if !output_code_string_counter >= 6 then begin
      output_char outchan '\n';
      output_code_string_counter := 0
    end
  done
(*e: function Bytelink.output_code_string *)

(*s: function Bytelink.output_data_string *)
(* Output a string as a C string *)

let output_data_string outchan data =
  let counter = ref 0 in
  output_string outchan "\"";
  for i = 0 to String.length data - 1 do
    Printf.fprintf outchan "\\%03o" (Char.code(data.[i]));
    incr counter;
    if !counter >= 16 then begin
      output_string outchan "\\\n";
      counter := 0
    end
  done;
  output_string outchan "\";\n\n"
(*e: function Bytelink.output_data_string *)

(*s: function Bytelink.link_bytecode_as_c *)
(* Output a bytecode executable as a C file *)

let link_bytecode_as_c objfiles outfile =
  let tolink = List.fold_right scan_file objfiles [] in
  let outchan = open_out outfile in
  try
    (* The bytecode *)
    output_string outchan "static int caml_code[] = {\n";
    Symtable.init();
    Hashtbl.clear crc_interfaces;
    let output_fun = output_code_string outchan
    and currpos_fun () = fatal_error "Bytelink.link_bytecode_as_c" in
    List.iter (link_file output_fun currpos_fun) tolink;
    (* The final STOP instruction *)
    Printf.fprintf outchan "\n0x%x};\n\n" Opcodes.opSTOP;
    (* The table of global data *)
    output_string outchan "static char * caml_data =\n";
    output_data_string outchan
      (Marshal.to_string (Symtable.initial_global_table()) []);
    (* The table of primitives *)
    Symtable.output_primitive_table outchan;
    (* The entry point *)
    output_string outchan "\n
void caml_startup(argv)
        char ** argv;
{
  caml_startup_code(caml_code, sizeof(caml_code), caml_data, argv);
}\n";
    close_out outchan
  with x ->
    close_out outchan;
    raise x
(*e: function Bytelink.link_bytecode_as_c *)

(*s: function Bytelink.extract *)
(* Build a custom runtime *)

let rec extract suffix l =
  match l with
  | [] -> []
  | h::t when Filename.check_suffix h suffix -> h :: (extract suffix t)
  | h::t -> extract suffix t
(*e: function Bytelink.extract *)
;;

(*s: function Bytelink.build_custom_runtime *)
let build_custom_runtime prim_name exec_name =
  let libname = "libcamlrun" ^ ext_lib in
  let runtime_lib =
    try
      find_in_path !load_path libname
    with Not_found ->
      raise(Error(File_not_found libname)) in
  match Sys.os_type with
    "Unix" ->
      Ccomp.command
       (Printf.sprintf
          "%s -o %s -I%s %s %s -L%s %s %s %s"
          Config.bytecomp_c_compiler
          exec_name
          Config.standard_library
          (String.concat " " (List.rev !Clflags.ccopts))
          prim_name
          Config.standard_library
          (String.concat " " (List.rev !Clflags.ccobjs))
          runtime_lib
          Config.c_libraries)
  | _ ->
    fatal_error "Bytelink.build_custom_runtime"
(*e: function Bytelink.build_custom_runtime *)

(*s: function Bytelink.append_bytecode_and_cleanup *)
let append_bytecode_and_cleanup bytecode_name exec_name prim_name =
  match Sys.os_type with
  | _ ->
      let oc =
        open_out_gen [Open_wronly; Open_append; Open_binary] 0
                                 !Clflags.exec_name in
      let ic = open_in_bin bytecode_name in
      copy_file ic oc;
      close_in ic;
      close_out oc;
      remove_file bytecode_name;
      remove_file prim_name
(*e: function Bytelink.append_bytecode_and_cleanup *)

(*s: function Bytelink.fix_exec_name *)
(* Fix the name of the output file, if the C compiler changes it behind
   our back. *)

let fix_exec_name name =
  match Sys.os_type with
  | _ -> name
(*e: function Bytelink.fix_exec_name *)

(*s: function Bytelink.link *)
(* Main entry point (build a custom runtime if needed) *)

let link objfiles =
  let objfiles = "stdlib.cma" :: (objfiles @ ["std_exit.cmo"]) in
  if not !Clflags.custom_runtime then
    link_bytecode objfiles !Clflags.exec_name true
  else if not !Clflags.output_c_object then begin
    let bytecode_name = Filename.temp_file "camlcode" "" in
    let prim_name = Filename.temp_file "camlprim" ".c" in
    try
      link_bytecode objfiles bytecode_name false;
      let poc = open_out prim_name in
      Symtable.output_primitive_table poc;
      close_out poc;
      let exec_name = fix_exec_name !Clflags.exec_name in
      if build_custom_runtime prim_name exec_name <> 0
      then raise(Error Custom_runtime);
      append_bytecode_and_cleanup bytecode_name exec_name prim_name
    with x ->
      remove_file bytecode_name;
      remove_file prim_name;
      raise x
  end else begin
    let c_file =
      Filename.chop_suffix !Clflags.object_name Config.ext_obj ^ ".c" in
    if Sys.file_exists c_file then raise(Error(File_exists c_file));
    try
      link_bytecode_as_c objfiles c_file;
      if Ccomp.compile_file_bytecode c_file <> 0
      then raise(Error Custom_runtime);
      remove_file c_file
    with x ->
      remove_file c_file;
      remove_file !Clflags.object_name;
      raise x
  end
(*e: function Bytelink.link *)

(* Error report *)

open Format

(*s: constant Bytelink.report_error *)
let report_error = function
    File_not_found name ->
      print_string "Cannot find file "; print_string name
  | Not_an_object_file name ->
      print_string "The file "; print_string name;
      print_string " is not a bytecode object file"
  | Symbol_error(name, err) ->
      print_string "Error while linking "; print_string name; print_string ":";
      print_space();
      Symtable.report_error err
  | Inconsistent_import(intf, file1, file2) ->
      open_hvbox 0;
      print_string "Files "; print_string file1; print_string " and ";
      print_string file2; print_space();
      print_string "make inconsistent assumptions over interface ";
      print_string intf;
      close_box()
  | Custom_runtime ->
      print_string "Error while building custom runtime system"
  | File_exists file ->
      print_string "Cannot overwrite existing file "; print_string file
(*e: constant Bytelink.report_error *)
(*e: ./bytecomp/bytelink.ml *)
