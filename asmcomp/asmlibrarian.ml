(*s: asmcomp/asmlibrarian.ml *)
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

(* Build libraries of .cmx files *)

open Misc
open Config
open Compilenv

(*s: type [[Asmlibrarian.error]]([[(asmcomp/asmlibrarian.ml)]]) *)
type error =
    File_not_found of string
  | Archiver_error of string
(*e: type [[Asmlibrarian.error]]([[(asmcomp/asmlibrarian.ml)]]) *)

(*s: exception [[Asmlibrarian.Error]]([[(asmcomp/asmlibrarian.ml)]]) *)
exception Error of error
(*e: exception [[Asmlibrarian.Error]]([[(asmcomp/asmlibrarian.ml)]]) *)

(*s: function [[Asmlibrarian.read_info]] *)
let read_info name =
  let filename =
    try
      find_in_path !load_path name
    with Not_found ->
      raise(Error(File_not_found name)) in
  let (info, crc) = Compilenv.read_unit_info filename in
  info.ui_force_link <- !Clflags.link_everything;
  (* There is no need to keep the approximation in the .cmxa file,
     since the compiler will go looking directly for .cmx files.
     The linker, which is the only one that reads .cmxa files, does not
     need the approximation. *)
  info.ui_approx <- Clambda.Value_unknown;
  (Filename.chop_suffix filename ".cmx" ^ ext_obj, (info, crc))
(*e: function [[Asmlibrarian.read_info]] *)

(*s: function [[Asmlibrarian.create_archive]] *)
let create_archive file_list lib_name =
  let archive_name = Filename.chop_suffix lib_name ".cmxa" ^ ext_lib in
  let outchan = open_out_bin lib_name in
  try
    output_string outchan cmxa_magic_number;
    let (objfile_list, descr_list) =
      List.split (List.map read_info file_list) in
    output_value outchan descr_list;
    if Ccomp.create_archive archive_name objfile_list <> 0
    then raise(Error(Archiver_error archive_name));
    close_out outchan
  with x ->
    close_out outchan;
    remove_file lib_name;
    remove_file archive_name;
    raise x
(*e: function [[Asmlibrarian.create_archive]] *)

open Format

(*s: function [[Asmlibrarian.report_error]] *)
let report_error = function
    File_not_found name ->
      print_string "Cannot find file "; print_string name
  | Archiver_error name ->
      print_string "Error while creating the library ";
      print_string name
(*e: function [[Asmlibrarian.report_error]] *)

(*e: asmcomp/asmlibrarian.ml *)
