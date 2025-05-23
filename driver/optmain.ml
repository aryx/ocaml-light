(*s: driver/optmain.ml *)
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

open Config
open Clflags

(*s: function [[Optmain.process_interface_file]] *)
let process_interface_file name =
  Optcompile.interface name
(*e: function [[Optmain.process_interface_file]] *)

(*s: function [[Optmain.process_implementation_file]] *)
let process_implementation_file name =
  Optcompile.implementation name;
  objfiles := (Filename.chop_extension name ^ ".cmx") :: !objfiles
(*e: function [[Optmain.process_implementation_file]] *)

(*s: function [[Optmain.process_file]] *)
let process_file name =
  Logs.info (fun m -> m "processing %s" name);
  if Filename.check_suffix name ".ml"
  or Filename.check_suffix name ".mlt" then begin
    Optcompile.implementation name;
    objfiles := (Filename.chop_extension name ^ ".cmx") :: !objfiles
  end
  else if Filename.check_suffix name ".mli" then
    Optcompile.interface name
  else if Filename.check_suffix name ".cmx" 
       or Filename.check_suffix name ".cmxa" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ext_obj
       or Filename.check_suffix name ext_lib then
    ccobjs := name :: !ccobjs
  else if Filename.check_suffix name ".c" then begin
    Optcompile.c_file name;
    ccobjs := (Filename.chop_suffix (Filename.basename name) ".c" ^ ext_obj)
    :: !ccobjs
  end
  else
    raise(Arg.Bad("don't know what to do with " ^ name))
(*e: function [[Optmain.process_file]] *)

(*s: function [[Optmain.print_version_number]] *)
let print_version_number () =
  print_string "The Objective Caml native-code compiler, version ";
  print_string Config.version; print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; print_newline()
(*e: function [[Optmain.print_version_number]] *)

(*s: constant [[Optmain.usage]] *)
let usage = "Usage: ocamlopt <options> <files>\nOptions are:"
(*e: constant [[Optmain.usage]] *)

(*s: function [[Optmain.main]] *)
let main () =
  try
    native_code := true;
    Arg.parse [
       "-a", Arg.Set make_archive, " Build a library";
       "-c", Arg.Set compile_only, " Compile only (do not link)";
       "-cclib", Arg.String(fun s -> ccobjs := s :: !ccobjs),
             "<opt>  Pass option <opt> to the C linker";
       "-ccopt", Arg.String(fun s -> ccopts := s :: !ccopts),
             "<opt>  Pass option <opt> to the C compiler and linker";
       "-compact", Arg.Clear optimize_for_speed,
             " Optimize code size rather than speed";
       "-i", Arg.Set print_types, " Print the types";
       "-I", Arg.String(fun dir -> include_dirs := dir :: !include_dirs),
             "<dir>  Add <dir> to the list of include directories";
       "-impl", Arg.String process_implementation_file,
             "<file>  Compile <file> as a .ml file";
       "-inline", Arg.Int(fun n -> inline_threshold := n * 8),
             "<n>  Set aggressiveness of inlining to <n>";
       "-intf", Arg.String process_interface_file,
             "<file>  Compile <file> as a .mli file";
       "-linkall", Arg.Set link_everything,
             " Link all modules, even unused ones";
       "-noassert", Arg.Set noassert, " Don't compile assertion checks";
       "-o", Arg.String(fun s -> exec_name := s;
                                 archive_name := s;
                                 object_name := s),
             "<file>  Set output file name to <file> (default a.out)";
       "-output-obj", Arg.Unit(fun () -> output_c_object := true),
             "Output a C object file instead of an executable";
       "-pp", Arg.String(fun s -> preprocessor := Some s),
             "<command>  Pipe sources through preprocessor <command>";
       "-S", Arg.Set keep_asm_file, " Keep intermediate assembly file";
       "-thread", Arg.Set thread_safe, " Use thread-safe standard library";
       "-unsafe", Arg.Set fast,
             " No bounds checking on array and string access";
       "-v", Arg.Unit print_version_number, " Print compiler version number";
       "-verbose", Arg.Unit (fun () ->
                       verbose := true;
                       Logs.set_level (Some Logs.Info)
                       ),
       " Print calls to external commands and info level for logs";
       (* new: *)
       "-debug", Arg.Unit (fun () ->
                       verbose := true;
                       Logs.set_level (Some Logs.Debug)
                     ),
       " debug level for logs";

       "-nopervasives", Arg.Set nopervasives, " (undocumented)";
       "-drawlambda", Arg.Set dump_rawlambda, " (undocumented)";
       "-dlambda", Arg.Set dump_lambda, " (undocumented)";
       "-dcmm", Arg.Set dump_cmm, " (undocumented)";
       "-dsel", Arg.Set dump_selection, " (undocumented)";
       "-dlive", Arg.Unit(fun () -> dump_live := true;
                                    Printmach.print_live := true),
             " (undocumented)";
       "-dspill", Arg.Set dump_spill, " (undocumented)";
       "-dsplit", Arg.Set dump_split, " (undocumented)";
       "-dinterf", Arg.Set dump_interf, " (undocumented)";
       "-dprefer", Arg.Set dump_prefer, " (undocumented)";
       "-dalloc", Arg.Set dump_regalloc, " (undocumented)";
       "-dreload", Arg.Set dump_reload, " (undocumented)";
       "-dscheduling", Arg.Set dump_scheduling, " (undocumented)";
       "-dlinear", Arg.Set dump_linear, " (undocumented)";
       "-dstartup", Arg.Set keep_startup_file, " (undocumented)";

       "-", Arg.String process_file,
            "<file>  Treat <file> as a file name (even if it starts with `-')"
      ] process_file usage;
    if !make_archive then begin
      Optcompile.init_path();
      Asmlibrarian.create_archive (List.rev !objfiles) !archive_name
    end
    else if not !compile_only & !objfiles <> [] then begin
      Optcompile.init_path();
      Asmlink.link (List.rev !objfiles)
    end;
    exit 0
  with x ->
    Format.set_formatter_out_channel stderr;
    Opterrors.report_error x;
    exit 2
(*e: function [[Optmain.main]] *)

(*s: toplevel [[Optmain._1]] *)
let _ = Printexc.catch main ()
(*e: toplevel [[Optmain._1]] *)
(*e: driver/optmain.ml *)
