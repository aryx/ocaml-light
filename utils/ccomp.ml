(*s: ./utils/ccomp.ml *)
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

(* $Id: ccomp.ml,v 1.3 1997/06/23 14:36:30 doligez Exp $ *)

(*s: function Ccomp.command *)
(* Compiling C files and building C libraries *)

let command cmdline =
  if !Clflags.verbose then begin
    prerr_string "+ ";
    prerr_string cmdline;
    prerr_newline()
  end;
  Sys.command cmdline
(*e: function Ccomp.command *)

(*s: function Ccomp.compile_file_bytecode *)
let compile_file_bytecode name =
  command
   (Printf.sprintf
     "%s -c %s %s -I%s %s"
     Config.bytecomp_c_compiler
     (String.concat " " (List.rev !Clflags.ccopts))
     (String.concat " "
       (List.map (fun dir -> "-I" ^ dir) 
                 (List.rev !Clflags.include_dirs)))
     Config.standard_library
     name)
(*e: function Ccomp.compile_file_bytecode *)

(*s: function Ccomp.compile_file_native *)
let compile_file_native name =
  command
   (Printf.sprintf
     "%s -c %s %s -I%s %s"
     Config.native_c_compiler
     (String.concat " " (List.rev !Clflags.ccopts))
     (String.concat " "
       (List.map (fun dir -> "-I" ^ dir) 
                 (List.rev !Clflags.include_dirs)))
     Config.standard_library
     name)
(*e: function Ccomp.compile_file_native *)

(*s: function Ccomp.create_archive *)
let create_archive archive file_list =
  Misc.remove_file archive;
  match Config.system with
    "win32" ->
      command(Printf.sprintf "lib /nologo /debugtype:cv /out:%s %s"
                                 archive (String.concat " " file_list))
  | _ ->
      let r1 =
        command(Printf.sprintf "ar rc %s %s"
                                   archive (String.concat " " file_list)) in
      if r1 <> 0 or String.length Config.ranlib = 0
      then r1
      else command(Config.ranlib ^ " " ^ archive)
(*e: function Ccomp.create_archive *)
(*e: ./utils/ccomp.ml *)
