(*s: driver/main.ml *)
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

(*s: function [[Main.process_interface_file]] *)
let process_interface_file name =
  Compile.interface name
(*e: function [[Main.process_interface_file]] *)

(*s: function [[Main.process_implementation_file]] *)
let process_implementation_file name =
  Compile.implementation name;
  objfiles := (Filename.chop_extension name ^ ".cmo") :: !objfiles
(*e: function [[Main.process_implementation_file]] *)

(*s: function [[Main.process_file]] *)
let process_file name =
  Logs.info (fun m -> m "processing %s" name);
  match () with
  | _ when Filename.check_suffix name ".ml" ->
      Compile.implementation name;
      objfiles := (Filename.chop_extension name ^ ".cmo") :: !objfiles

  | _ when Filename.check_suffix name ".mli" ->
      Compile.interface name

  (*s: [[Main.process_file()]] cases *)
  | _ when Filename.check_suffix name ".cmo" 
        or Filename.check_suffix name ".cma" ->
      objfiles := name :: !objfiles
  (*x: [[Main.process_file()]] cases *)
  | _ when Filename.check_suffix name Config.ext_obj
        or Filename.check_suffix name Config.ext_lib ->
      ccobjs := name :: !ccobjs
  (*x: [[Main.process_file()]] cases *)
  | _ when Filename.check_suffix name ".c" ->
      Compile.c_file name;
      ccobjs := (Filename.chop_suffix (Filename.basename name) ".c" ^ ext_obj)
       :: !ccobjs
  (*e: [[Main.process_file()]] cases *)
  | _ -> 
      raise(Arg.Bad("don't know what to do with " ^ name))
(*e: function [[Main.process_file]] *)

(*s: function [[Main.print_version_number]] *)
let print_version_number () =
  print_string "The Objective Caml compiler, version ";
  print_string Config.version; 
  print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; 
  print_newline()
(*e: function [[Main.print_version_number]] *)

(*s: constant [[Main.usage]] *)
let usage = "Usage: ocamlc <options> <files>\nOptions are:"
(*e: constant [[Main.usage]] *)

(*s: function [[Main.main]] *)
let main () =
  try
    Arg.parse [
       (*s: [[Main.main()]] command line options *)
       "-nopervasives", Arg.Set nopervasives, " (undocumented)";
       (*x: [[Main.main()]] command line options *)
       "-o", Arg.String(fun s -> exec_name := s;
                                 archive_name := s;
                                 object_name := s),
             "<file>  Set output file name to <file> (default a.out)";
       (*x: [[Main.main()]] command line options *)
       "-v", Arg.Unit print_version_number, " Print compiler version number";
       (*x: [[Main.main()]] command line options *)
       "-", Arg.String process_file,
            "<file>  Treat <file> as a file name (even if it starts with `-')";
       (*x: [[Main.main()]] command line options *)
       "-impl", Arg.String process_implementation_file,
             "<file>  Compile <file> as a .ml file";
       (*x: [[Main.main()]] command line options *)
       "-intf", Arg.String process_interface_file,
             "<file>  Compile <file> as a .mli file";
       (*x: [[Main.main()]] command line options *)
       "-c", Arg.Set compile_only, " Compile only (do not link)";
       (*x: [[Main.main()]] command line options *)
       "-I", Arg.String(fun dir -> include_dirs := dir :: !include_dirs),
             "<dir>  Add <dir> to the list of include directories";
       (*x: [[Main.main()]] command line options *)
       "-a", Arg.Set make_archive, " Build a library";
       (*x: [[Main.main()]] command line options *)
       "-cclib", Arg.String(fun s -> ccobjs := s :: !ccobjs),
             "<opt>  Pass option <opt> to the C linker";
       (*x: [[Main.main()]] command line options *)
       "-custom", Arg.Set custom_runtime, " Link in custom mode";
       (*x: [[Main.main()]] command line options *)
       "-ccopt", Arg.String(fun s -> ccopts := s :: !ccopts),
             "<opt>  Pass option <opt> to the C compiler and linker";
       (*x: [[Main.main()]] command line options *)
       "-linkall", Arg.Set link_everything,
             " Link all modules, even unused ones";
       (*x: [[Main.main()]] command line options *)
       "-output-obj", Arg.Unit(fun () -> output_c_object := true;
                                         custom_runtime := true),
             "Output a C object file instead of an executable";
       (*x: [[Main.main()]] command line options *)
       "-g", Arg.Set debug, " Save debugging information";
       (*x: [[Main.main()]] command line options *)
       "-unsafe", Arg.Set fast,
             " No bounds checking on array and string access";
       (*x: [[Main.main()]] command line options *)
       "-noassert", Arg.Set noassert, " Don't compile assertion checks";
       (*x: [[Main.main()]] command line options *)
       "-thread", Arg.Set thread_safe, " Use thread-safe standard library";
       (*x: [[Main.main()]] command line options *)
       "-i", Arg.Set print_types, " Print the types";
       (*x: [[Main.main()]] command line options *)
       "-pp", Arg.String(fun s -> preprocessor := Some s),
             "<command>  Pipe sources through preprocessor <command>";
       (*x: [[Main.main()]] command line options *)
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
       (*x: [[Main.main()]] command line options *)
       "-dlambda", Arg.Set dump_lambda, " (undocumented)";
       "-drawlambda", Arg.Set dump_rawlambda, " (undocumented)";
       (*x: [[Main.main()]] command line options *)
       "-dinstr", Arg.Set dump_instr, " (undocumented)";
       (*e: [[Main.main()]] command line options *)

       "-output-c", Arg.Unit(fun () -> output_c := true;
                                       custom_runtime := true),
             "Output a C file instead of an executable";
      ] 
      process_file 
      usage;

    (match () with
    (*s: [[Main.main()]] if make archive case *)
    | _ when !make_archive ->
        Compile.init_path();
        Bytelibrarian.create_archive (List.rev !objfiles) !archive_name
    (*e: [[Main.main()]] if make archive case *)
    (*s: [[Main.main()]] if linking case *)
    | _ when not !compile_only & !objfiles <> [] ->
        Compile.init_path();
        Bytelink.link (List.rev !objfiles)
    (*e: [[Main.main()]] if linking case *)
    | _ -> ()
    );
    exit 0
  with x ->
    Format.set_formatter_out_channel stderr;
    Errors.report_error x;
    exit 2
(*e: function [[Main.main]] *)

(*s: toplevel [[Main._1]] *)
let _ = 
  Printexc.catch main ()
(*e: toplevel [[Main._1]] *)
(*e: driver/main.ml *)
