(*s: ./utils/config.mli *)
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

(* $Id: config.mli,v 1.14 1997/03/17 13:01:04 xleroy Exp $ *)

(*s: signature Config.version *)
(* System configuration *)

val version: string
        (* The current version number of the system *)
(*e: signature Config.version *)

(*s: signature Config.standard_library *)
val standard_library: string
        (* The directory containing the standard libraries *)
(*e: signature Config.standard_library *)
(*s: signature Config.bytecomp_c_compiler *)
val bytecomp_c_compiler: string
        (* The C compiler to use for the custom runtime mode of the
           bytecode compiler *)
(*e: signature Config.bytecomp_c_compiler *)
(*s: signature Config.native_c_compiler *)
val native_c_compiler: string
        (* The C compiler to use for the native code compiler *)
(*e: signature Config.native_c_compiler *)
(*s: signature Config.native_partial_linker *)
val native_partial_linker: string
        (* The linker to use for partial links (-output-obj option) *)
(*e: signature Config.native_partial_linker *)
(*s: signature Config.c_libraries *)
val c_libraries: string
        (* The C libraries to link with custom runtimes *)
(*e: signature Config.c_libraries *)
(*s: signature Config.ranlib *)
val ranlib: string
        (* Command to randomize a library, or "" if not needed *)
(*e: signature Config.ranlib *)

(*s: signature Config.load_path *)
val load_path: string list ref
        (* Directories in the search path for .cmi and .cmo files *)
(*e: signature Config.load_path *)

(*s: signature Config.exec_magic_number *)
val exec_magic_number: string
        (* Magic number for bytecode executable files *)
(*e: signature Config.exec_magic_number *)
(*s: signature Config.cmi_magic_number *)
val cmi_magic_number: string
        (* Magic number for compiled interface files *)
(*e: signature Config.cmi_magic_number *)
(*s: signature Config.cmo_magic_number *)
val cmo_magic_number: string
        (* Magic number for object bytecode files *)
(*e: signature Config.cmo_magic_number *)
(*s: signature Config.cma_magic_number *)
val cma_magic_number: string
        (* Magic number for archive files *)
(*e: signature Config.cma_magic_number *)
(*s: signature Config.cmx_magic_number *)
val cmx_magic_number: string
        (* Magic number for compilation unit descriptions *)
(*e: signature Config.cmx_magic_number *)
(*s: signature Config.cmxa_magic_number *)
val cmxa_magic_number: string
        (* Magic number for libraries of compilation unit descriptions *)
(*e: signature Config.cmxa_magic_number *)
(*s: signature Config.ast_intf_magic_number *)
val ast_intf_magic_number: string
        (* Magic number for file holding an interface syntax tree *)
(*e: signature Config.ast_intf_magic_number *)
(*s: signature Config.ast_impl_magic_number *)
val ast_impl_magic_number: string
        (* Magic number for file holding an implementation syntax tree *)
(*e: signature Config.ast_impl_magic_number *)

(*s: signature Config.max_tag *)
val max_tag: int
        (* Biggest tag that can be stored in the header of a block. *)
(*e: signature Config.max_tag *)
(*s: signature Config.max_young_wosize *)
val max_young_wosize: int
        (* Maximal size of arrays that are directly allocated in the
           minor heap *)
(*e: signature Config.max_young_wosize *)
(*s: signature Config.architecture *)
val architecture: string
        (* Name of processor type for the native-code compiler *)
(*e: signature Config.architecture *)
(*s: signature Config.model *)
val model: string
        (* Name of processor submodel for the native-code compiler *)
(*e: signature Config.model *)
(*s: signature Config.system *)
val system: string
        (* Name of operating system for the native-code compiler *)
(*e: signature Config.system *)

(*s: signature Config.ext_obj *)
val ext_obj: string
        (* Extension for object files, e.g. [.o] under Unix. *)
(*e: signature Config.ext_obj *)
(*s: signature Config.ext_asm *)
val ext_asm: string
        (* Extension for assembler files, e.g. [.s] under Unix. *)
(*e: signature Config.ext_asm *)
(*s: signature Config.ext_lib *)
val ext_lib: string
        (* Extension for library files, e.g. [.a] under Unix. *)
(*e: signature Config.ext_lib *)
(*e: ./utils/config.mli *)
