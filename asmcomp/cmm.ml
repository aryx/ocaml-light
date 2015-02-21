(*s: asmcomp/cmm.ml *)
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

(*s: type Cmm.machtype_component (asmcomp/cmm.ml) *)
(* $Id: cmm.ml,v 1.11 1997/03/04 10:19:43 xleroy Exp $ *)

type machtype_component =
    Addr
  | Int
  | Float
(*e: type Cmm.machtype_component (asmcomp/cmm.ml) *)

(*s: type Cmm.machtype (asmcomp/cmm.ml) *)
type machtype = machtype_component array
(*e: type Cmm.machtype (asmcomp/cmm.ml) *)

(*s: constant Cmm.typ_void *)
let typ_void = ([||] : machtype_component array)
(*e: constant Cmm.typ_void *)
(*s: constant Cmm.typ_addr *)
let typ_addr = [|Addr|]
(*e: constant Cmm.typ_addr *)
(*s: constant Cmm.typ_int *)
let typ_int = [|Int|]
(*e: constant Cmm.typ_int *)
(*s: constant Cmm.typ_float *)
let typ_float = [|Float|]
(*e: constant Cmm.typ_float *)

(*s: function Cmm.size_component *)
let size_component = function
    Addr -> Arch.size_addr
  | Int -> Arch.size_int
  | Float -> Arch.size_float
(*e: function Cmm.size_component *)

(*s: function Cmm.size_machtype *)
let size_machtype mty =
  let size = ref 0 in
  for i = 0 to Array.length mty - 1 do
    size := !size + size_component mty.(i)
  done;
  !size
(*e: function Cmm.size_machtype *)

(*s: type Cmm.comparison (asmcomp/cmm.ml) *)
type comparison =
    Ceq
  | Cne
  | Clt
  | Cle
  | Cgt
  | Cge
(*e: type Cmm.comparison (asmcomp/cmm.ml) *)

(*s: function Cmm.negate_comparison *)
let negate_comparison = function
    Ceq -> Cne | Cne -> Ceq
  | Clt -> Cge | Cle -> Cgt
  | Cgt -> Cle | Cge -> Clt
(*e: function Cmm.negate_comparison *)

(*s: function Cmm.swap_comparison *)
let swap_comparison = function
    Ceq -> Ceq | Cne -> Cne
  | Clt -> Cgt | Cle -> Cge
  | Cgt -> Clt | Cge -> Cle
(*e: function Cmm.swap_comparison *)

(*s: type Cmm.memory_chunk (asmcomp/cmm.ml) *)
type memory_chunk =
    Byte_unsigned
  | Byte_signed
  | Sixteen_unsigned
  | Sixteen_signed
  | Word
(*e: type Cmm.memory_chunk (asmcomp/cmm.ml) *)

(*s: type Cmm.operation (asmcomp/cmm.ml) *)
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
(*e: type Cmm.operation (asmcomp/cmm.ml) *)

(*s: type Cmm.expression (asmcomp/cmm.ml) *)
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
(*e: type Cmm.expression (asmcomp/cmm.ml) *)

(*s: type Cmm.fundecl (asmcomp/cmm.ml) *)
type fundecl =
  { fun_name: string;
    fun_args: (Ident.t * machtype) list;
    fun_body: expression;
    fun_fast: bool }
(*e: type Cmm.fundecl (asmcomp/cmm.ml) *)

(*s: type Cmm.data_item (asmcomp/cmm.ml) *)
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
(*e: type Cmm.data_item (asmcomp/cmm.ml) *)

(*s: type Cmm.phrase (asmcomp/cmm.ml) *)
type phrase =
    Cfunction of fundecl
  | Cdata of data_item list
(*e: type Cmm.phrase (asmcomp/cmm.ml) *)

(*e: asmcomp/cmm.ml *)
