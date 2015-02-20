(*s: ./utils/clflags.ml *)
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

(* Command-line parameters *)

let objfiles = ref ([] : string list)   (* .cmo and .cma files *)
and ccobjs = ref ([] : string list)     (* .o, .a and -lxxx files *)

let compile_only = ref false            (* -c *)
and exec_name = ref "a.out"             (* -o *)
and archive_name = ref "library.cma"    (* -o *)
and object_name = ref ("camlprog" ^ Config.ext_obj) (* -o *)
and include_dirs = ref ([] : string list)(* -I *)
and print_types = ref false             (* -i *)
and make_archive = ref false            (* -a *)
and debug = ref false                   (* -g *)
and fast = ref false                    (* -unsafe *)
and link_everything = ref false         (* -linkall *)
and custom_runtime = ref false          (* -custom *)
and output_c_object = ref false         (* -output-obj *)
and ccopts = ref ([] : string list)     (* -ccopt *)
and nopervasives = ref false            (* -nopervasives *)
and preprocessor = ref(None : string option) (* -pp *)
and thread_safe = ref false             (* -thread *)
and noassert = ref false                (* -noassert *)
and verbose = ref false                 (* -verbose *)

let dump_rawlambda = ref false          (* -drawlambda *)
and dump_lambda = ref false             (* -dlambda *)
and dump_instr = ref false              (* -dinstr *)

(*s: constant Clflags.keep_asm_file *)
let keep_asm_file = ref false           (* -S *)
(*e: constant Clflags.keep_asm_file *)
let optimize_for_speed = ref true       (* -compact *)

and dump_cmm = ref false                (* -dcmm *)
(*s: constant Clflags.dump_selection *)
let dump_selection = ref false          (* -dsel *)
(*e: constant Clflags.dump_selection *)
(*s: constant Clflags.dump_live *)
let dump_live = ref false               (* -dlive *)
(*e: constant Clflags.dump_live *)
(*s: constant Clflags.dump_spill *)
let dump_spill = ref false              (* -dspill *)
(*e: constant Clflags.dump_spill *)
(*s: constant Clflags.dump_split *)
let dump_split = ref false              (* -dsplit *)
(*e: constant Clflags.dump_split *)
(*s: constant Clflags.dump_scheduling *)
let dump_scheduling = ref false         (* -dscheduling *)
(*e: constant Clflags.dump_scheduling *)
(*s: constant Clflags.dump_interf *)
let dump_interf = ref false             (* -dinterf *)
(*e: constant Clflags.dump_interf *)
(*s: constant Clflags.dump_prefer *)
let dump_prefer = ref false             (* -dprefer *)
(*e: constant Clflags.dump_prefer *)
(*s: constant Clflags.dump_regalloc *)
let dump_regalloc = ref false           (* -dalloc *)
(*e: constant Clflags.dump_regalloc *)
(*s: constant Clflags.dump_reload *)
let dump_reload = ref false             (* -dreload *)
(*e: constant Clflags.dump_reload *)
(*s: constant Clflags.dump_scheduling (./utils/clflags.ml) *)
let dump_scheduling = ref false         (* -dscheduling *)
(*e: constant Clflags.dump_scheduling (./utils/clflags.ml) *)
(*s: constant Clflags.dump_linear *)
let dump_linear = ref false             (* -dlinear *)
(*e: constant Clflags.dump_linear *)
(*s: constant Clflags.keep_startup_file *)
let keep_startup_file = ref false       (* -dstartup *)
(*e: constant Clflags.keep_startup_file *)

(*s: constant Clflags.native_code *)
let native_code = ref false             (* set to true under ocamlopt *)
(*e: constant Clflags.native_code *)

(*s: constant Clflags.inline_threshold *)
let inline_threshold = ref 10
(*e: constant Clflags.inline_threshold *)
(*e: ./utils/clflags.ml *)
