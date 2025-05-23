(*s: driver/compile.ml *)
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

(* The batch compiler *)

open Misc
open Config
open Format
open Typedtree

(*s: function [[Compile.init_path]] *)
(* Initialize the search path.
   The current directory is always searched first,
   then the directories specified with the -I option (in command-line order),
   then the standard library directory. *)

let init_path () =
  let dirs =
    if !Clflags.thread_safe then
     Filename.concat Config.standard_library "threads" :: !Clflags.include_dirs
    else
     !Clflags.include_dirs in
  load_path := "" :: List.rev (Config.standard_library :: dirs);
  Env.reset_cache()
(*e: function [[Compile.init_path]] *)

(*s: function [[Compile.initial_env]] *)
(* Return the initial environment in which compilation proceeds. *)

let initial_env () =
  try
    if !Clflags.nopervasives
    then Env.initial
    else Env.open_pers_signature "Pervasives" Env.initial
  with Not_found ->
    fatal_error "cannot open Pervasives.cmi"
(*e: function [[Compile.initial_env]] *)

(*s: function [[Compile.preprocess]] *)
(* Optionally preprocess a source file *)

let preprocess sourcefile tmpfile =
  match !Clflags.preprocessor with
    None -> sourcefile
  | Some pp ->
      let comm = pp ^ " " ^ sourcefile ^ " > " ^ tmpfile in
      if Ccomp.command comm <> 0 then begin
        Printf.eprintf "Preprocessing error\n";
        flush stderr;
        exit 2
      end;
      tmpfile
(*e: function [[Compile.preprocess]] *)

(*s: function [[Compile.remove_preprocessed]] *)
let remove_preprocessed inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ -> remove_file inputfile
(*e: function [[Compile.remove_preprocessed]] *)

(*s: exception [[Compile.Outdated_version]] *)
(* Parse a file or get a dumped syntax tree in it *)

exception Outdated_version
(*e: exception [[Compile.Outdated_version]] *)

(*s: function [[Compile.parse_file]] *)
let parse_file inputfile parse_fun ast_magic =
  let ic = open_in inputfile in
  (*s: [[Compile.parse_file()]] let [[is_ast_file]] *)
  let is_ast_file =
    try
      let buffer = Bytes.create (String.length ast_magic) in
      really_input ic buffer 0 (String.length ast_magic);
      if Bytes.to_string buffer = ast_magic then true
      else if String.sub (Bytes.to_string buffer) 0 9 = String.sub ast_magic 0 9 then
        raise Outdated_version
      else false
    with
      Outdated_version ->
        fatal_error "Ocaml and preprocessor have incompatible versions"
    | _ -> false
  in
  (*e: [[Compile.parse_file()]] let [[is_ast_file]] *)
  let ast =
    try
      (*s: [[Compile.parse_file()]] if [[is_ast_file]] *)
      if is_ast_file then begin
        Location.input_name := input_value ic;
        input_value ic
      end 
      (*e: [[Compile.parse_file()]] if [[is_ast_file]] *)
      else begin
        seek_in ic 0;
        Location.input_name := inputfile;
        parse_fun (Lexing.from_channel ic)
      end
    with x -> close_in ic; raise x
  in
  close_in ic;
  ast
(*e: function [[Compile.parse_file]] *)

(*s: function [[Compile.interface]] *)
(* Compile a .mli file *)

let interface sourcefile =
  init_path();
  let prefixname = Filename.chop_extension sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in
  let inputfile = preprocess sourcefile (prefixname ^ ".ppi") in

  (* parsing *)
  let ast = parse_file inputfile Parse.interface ast_intf_magic_number in

  (* typing *)
  let sg = Typemod.transl_signature (initial_env()) ast in

  if !Clflags.print_types 
  then (Printtyp.signature sg; print_newline());

  Env.save_signature sg modulename (prefixname ^ ".cmi") |> ignore;
  remove_preprocessed inputfile
(*e: function [[Compile.interface]] *)

(*s: function [[Compile.print_if]] *)
(* Compile a .ml file *)

let print_if flag printer arg =
  if !flag then begin printer arg; print_newline() end;
  arg
(*e: function [[Compile.print_if]] *)

(*s: function [[Compile.implementation]] *)
let implementation sourcefile =
  init_path();
  let prefixname = Filename.chop_extension sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in
  let inputfile = preprocess sourcefile (prefixname ^ ".ppo") in

  (* parsing *)
  let ast = parse_file inputfile Parse.implementation ast_impl_magic_number in

  let objfile = prefixname ^ ".cmo" in
  let oc = open_out objfile in
  try

    (* typing *)
    let (struc, sg, finalenv) =
      Typemod.type_structure (initial_env()) ast in

    if !Clflags.print_types 
    then (Printtyp.signature sg; print_newline());

    (* checking *)
    let (coercion, _crc) =
      if Sys.file_exists (prefixname ^ ".mli") then begin
        let intf_file =
          try find_in_path !load_path (prefixname ^ ".cmi")
          with Not_found -> prefixname ^ ".cmi" 
        in
        let (dclsig, crc) = Env.read_signature modulename intf_file in
        (Includemod.compunit sourcefile sg intf_file dclsig, crc)
      end else begin
        let crc = Env.save_signature sg modulename (prefixname ^ ".cmi") in
        Typemod.check_nongen_schemes struc;
        (Tcoerce_none, crc)
      end 
    in

    (* generating *)
    Translmod.transl_implementation modulename struc coercion
    |> print_if Clflags.dump_rawlambda Printlambda.lambda
    |> Simplif.simplify_lambda
    |> print_if Clflags.dump_lambda Printlambda.lambda
    |> Bytegen.compile_implementation modulename
    |> print_if Clflags.dump_instr Printinstr.instrlist
    |> Emitcode.to_file oc modulename
    ;

    remove_preprocessed inputfile;
    close_out oc
  with x ->
    close_out oc;
    remove_file objfile;
    raise x
(*e: function [[Compile.implementation]] *)

(*s: function [[Compile.c_file]] *)
let c_file name =
  if Ccomp.compile_file_bytecode name <> 0 
  then exit 2
(*e: function [[Compile.c_file]] *)
(*e: driver/compile.ml *)
