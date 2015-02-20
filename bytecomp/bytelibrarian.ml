(*s: ./bytecomp/bytelibrarian.ml *)
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

(* Build libraries of .cmo files *)

open Misc
open Config
open Emitcode

(*s: type Bytelibrarian.error *)
type error =
    File_not_found of string
  | Not_an_object_file of string
(*e: type Bytelibrarian.error *)

(*s: exception Bytelibrarian.Error *)
exception Error of error
(*e: exception Bytelibrarian.Error *)

(*s: function Bytelibrarian.copy_compunit *)
let copy_compunit ic oc compunit =
  seek_in ic compunit.cu_pos;
  compunit.cu_pos <- pos_out oc;
  compunit.cu_force_link <- !Clflags.link_everything;
  copy_file_chunk ic oc compunit.cu_codesize;
  if compunit.cu_debug > 0 then begin
    seek_in ic compunit.cu_debug;
    compunit.cu_debug <- pos_out oc;
    copy_file_chunk ic oc compunit.cu_debugsize
  end
(*e: function Bytelibrarian.copy_compunit *)

(*s: function Bytelibrarian.copy_object_file *)
let copy_object_file oc name =
  let file_name =
    try
      find_in_path !load_path name
    with Not_found ->
      raise(Error(File_not_found name)) in
  let ic = open_in_bin file_name in
  try
    let buffer = String.create (String.length cmo_magic_number) in
    really_input ic buffer 0 (String.length cmo_magic_number);
    if buffer = cmo_magic_number then begin
      let compunit_pos = input_binary_int ic in
      seek_in ic compunit_pos;
      let compunit = (input_value ic : compilation_unit) in
      copy_compunit ic oc compunit;
      close_in ic;
      [compunit]
    end else
    if buffer = cma_magic_number then begin
      let toc_pos = input_binary_int ic in
      seek_in ic toc_pos;
      let toc = (input_value ic : compilation_unit list) in
      List.iter (copy_compunit ic oc) toc;
      close_in ic;
      toc
    end else
      raise(Error(Not_an_object_file file_name))
  with x ->
    close_in ic;
    raise x
(*e: function Bytelibrarian.copy_object_file *)

(*s: function Bytelibrarian.create_archive *)
let create_archive file_list lib_name =
  let outchan = open_out_bin lib_name in
  try
    output_string outchan cma_magic_number;
    let ofs_pos_toc = pos_out outchan in
    output_binary_int outchan 0;
    let toc = List.flatten(List.map (copy_object_file outchan) file_list) in
    let pos_toc = pos_out outchan in
    output_value outchan toc;
    seek_out outchan ofs_pos_toc;
    output_binary_int outchan pos_toc;
    close_out outchan
  with x ->
    close_out outchan;
    remove_file lib_name;
    raise x
(*e: function Bytelibrarian.create_archive *)

open Format

(*s: constant Bytelibrarian.report_error *)
let report_error = function
    File_not_found name ->
      print_string "Cannot find file "; print_string name
  | Not_an_object_file name ->
      print_string "The file "; print_string name;
      print_string " is not a bytecode object file"
(*e: constant Bytelibrarian.report_error *)

(*e: ./bytecomp/bytelibrarian.ml *)
