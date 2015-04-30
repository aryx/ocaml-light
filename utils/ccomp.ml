(*s: ./utils/ccomp.ml *)
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

(* Compiling C files and building C libraries *)

(*s: function Ccomp.command *)
let command cmdline =
  if !Clflags.verbose then begin
    prerr_string "+ ";
    prerr_string cmdline;
    prerr_newline()
  end;
  let ret = Sys.command cmdline in
  if !Clflags.verbose then begin
    prerr_string "- ";
    prerr_int ret;
    prerr_newline()
  end;
  ret
  
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
  | _ ->
      let r1 =
        command(Printf.sprintf "ar rc %s %s"
                                   archive (String.concat " " file_list)) in
      if r1 <> 0 or String.length Config.ranlib = 0
      then r1
      else command(Config.ranlib ^ " " ^ archive)
(*e: function Ccomp.create_archive *)
(*e: ./utils/ccomp.ml *)
