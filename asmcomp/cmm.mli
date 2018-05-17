(*s: asmcomp/cmm.mli *)
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

(*s: type [[Cmm.machtype_component]] *)
(* Second intermediate language (machine independent) *)

type machtype_component =
    Addr
  | Int
  | Float
(*e: type [[Cmm.machtype_component]] *)

(*s: type [[Cmm.machtype]] *)
type machtype = machtype_component array
(*e: type [[Cmm.machtype]] *)

(*s: signature [[Cmm.typ_void]] *)
val typ_void: machtype
(*e: signature [[Cmm.typ_void]] *)
(*s: signature [[Cmm.typ_addr]] *)
val typ_addr: machtype
(*e: signature [[Cmm.typ_addr]] *)
(*s: signature [[Cmm.typ_int]] *)
val typ_int: machtype
(*e: signature [[Cmm.typ_int]] *)
(*s: signature [[Cmm.typ_float]] *)
val typ_float: machtype
(*e: signature [[Cmm.typ_float]] *)

(*s: signature [[Cmm.size_component]] *)
val size_component: machtype_component -> int
(*e: signature [[Cmm.size_component]] *)
(*s: signature [[Cmm.size_machtype]] *)
val size_machtype: machtype -> int
(*e: signature [[Cmm.size_machtype]] *)

(*s: type [[Cmm.comparison]] *)
type comparison =
    Ceq
  | Cne
  | Clt
  | Cle
  | Cgt
  | Cge
(*e: type [[Cmm.comparison]] *)

(*s: signature [[Cmm.negate_comparison]] *)
val negate_comparison: comparison -> comparison
(*e: signature [[Cmm.negate_comparison]] *)
(*s: signature [[Cmm.swap_comparison]] *)
val swap_comparison: comparison -> comparison
(*e: signature [[Cmm.swap_comparison]] *)

(*s: type [[Cmm.memory_chunk]] *)
type memory_chunk =
    Byte_unsigned
  | Byte_signed
  | Sixteen_unsigned
  | Sixteen_signed
  | Word
(*e: type [[Cmm.memory_chunk]] *)

(*s: type [[Cmm.operation]] *)
type operation =
    Capply of machtype
  | Cextcall of string * machtype * bool
  | Cproj of int * int
  | Cload of machtype
  | Cloadchunk of memory_chunk
  | Calloc
  | Cstore
  | Cstorechunk of memory_chunk
  | Caddi | Csubi | Cmuli | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  | Ccmpi of comparison
  | Cadda | Csuba
  | Ccmpa of comparison
  | Cnegf | Cabsf
  | Caddf | Csubf | Cmulf | Cdivf
  | Cfloatofint | Cintoffloat
  | Ccmpf of comparison
  | Craise
  | Ccheckbound
(*e: type [[Cmm.operation]] *)

(*s: type [[Cmm.expression]] *)
type expression =
    Cconst_int of int
  | Cconst_natint of Nativeint.t
  | Cconst_float of string
  | Cconst_symbol of string
  | Cconst_pointer of int
  | Cvar of Ident.t
  | Clet of Ident.t * expression * expression
  | Cassign of Ident.t * expression
  | Ctuple of expression list
  | Cop of operation * expression list
  | Csequence of expression * expression
  | Cifthenelse of expression * expression * expression
  | Cswitch of expression * int array * expression array
  | Cloop of expression
  | Ccatch of expression * expression
  | Cexit
  | Ctrywith of expression * Ident.t * expression
(*e: type [[Cmm.expression]] *)

(*s: type [[Cmm.fundecl]] *)
type fundecl =
  { fun_name: string;
    fun_args: (Ident.t * machtype) list;
    fun_body: expression;
    fun_fast: bool }
(*e: type [[Cmm.fundecl]] *)

(*s: type [[Cmm.data_item]] *)
type data_item =
    Cdefine_symbol of string
  | Cdefine_label of int
  | Cint8 of int
  | Cint16 of int
  | Cint of Nativeint.t
  | Cfloat of string
  | Csymbol_address of string
  | Clabel_address of int
  | Cstring of string
  | Cskip of int
  | Calign of int
(*e: type [[Cmm.data_item]] *)

(*s: type [[Cmm.phrase]] *)
type phrase =
    Cfunction of fundecl
  | Cdata of data_item list
(*e: type [[Cmm.phrase]] *)

(*e: asmcomp/cmm.mli *)
