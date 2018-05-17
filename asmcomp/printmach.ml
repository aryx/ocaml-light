(*s: asmcomp/printmach.ml *)
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

(* Pretty-printing of pseudo machine code *)

open Format
open Cmm
open Reg
open Mach

(*s: function [[Printmach.reg]] *)
let reg r =
  if String.length r.name > 0 then
    print_string r.name
  else
    print_string(match r.typ with Addr -> "A" | Int -> "I" | Float -> "F");
  print_string "/";
  print_int r.stamp;
  begin match r.loc with
    Unknown -> ()
  | Reg r -> 
      print_string "["; print_string(Proc.register_name r); print_string "]"
  | Stack(Local s) ->
      print_string "[s"; print_int s; print_string "]"
  | Stack(Incoming s) ->
      print_string "[si"; print_int s; print_string "]"
  | Stack(Outgoing s) ->
      print_string "[so"; print_int s; print_string "]"
  end
(*e: function [[Printmach.reg]] *)

(*s: function [[Printmach.regs]] *)
let regs v =
  match Array.length v with
    0 -> ()
  | 1 -> reg v.(0)
  | n -> reg v.(0);
         for i = 1 to n-1 do print_string " "; reg v.(i) done
(*e: function [[Printmach.regs]] *)

(*s: function [[Printmach.regset]] *)
let regset s =
  let first = ref true in
  (*Reg.*)Set.iter
    (fun r ->
      if !first then first := false else print_space();
      reg r)
    s
(*e: function [[Printmach.regset]] *)

(*s: function [[Printmach.regsetaddr]] *)
let regsetaddr s =
  let first = ref true in
  (*Reg.*)Set.iter
    (fun r ->
      if !first then first := false else print_space();
      reg r;
      match r.typ with Addr -> print_string "*" | _ -> ())
    s
(*e: function [[Printmach.regsetaddr]] *)

(*s: function [[Printmach.intcomp]] *)
let intcomp = function
    Isigned c -> print_string " "; Printcmm.comparison c; print_string "s "
  | Iunsigned c -> print_string " "; Printcmm.comparison c; print_string "u "
(*e: function [[Printmach.intcomp]] *)

(*s: function [[Printmach.floatcomp]] *)
let floatcomp c =
    print_string " "; Printcmm.comparison c; print_string "f "
(*e: function [[Printmach.floatcomp]] *)

(*s: function [[Printmach.intop]] *)
let intop = function
    Iadd -> print_string " + "
  | Isub -> print_string " - "
  | Imul -> print_string " * "
  | Idiv -> print_string " div "
  | Imod -> print_string " mod "
  | Iand -> print_string " & "
  | Ior -> print_string " | "
  | Ixor -> print_string " ^ "
  | Ilsl -> print_string " << "
  | Ilsr -> print_string " >>u "
  | Iasr -> print_string " >>s "
  | Icomp cmp -> intcomp cmp
  | Icheckbound -> print_string " check > "
(*e: function [[Printmach.intop]] *)
    
(*s: function [[Printmach.test]] *)
let test tst arg =
  match tst with
    Itruetest -> reg arg.(0)
  | Ifalsetest -> print_string "not "; reg arg.(0)
  | Iinttest cmp -> reg arg.(0); intcomp cmp; reg arg.(1)
  | Iinttest_imm(cmp, n) -> reg arg.(0); intcomp cmp; print_int n
  | Ifloattest(cmp, neg) ->
      if neg then print_string "not ";
      reg arg.(0); floatcomp cmp; reg arg.(1)
  | Ieventest -> reg arg.(0); print_string " & 1 == 0"
  | Ioddtest -> reg arg.(0); print_string " & 1 == 1"
(*e: function [[Printmach.test]] *)

(*s: constant [[Printmach.print_live]] *)
let print_live = ref false
(*e: constant [[Printmach.print_live]] *)

(*s: function [[Printmach.operation]] *)
let operation op arg res =
  if Array.length res > 0 then begin regs res; print_string " := " end;
  match op with
    Imove -> regs arg
  | Ispill -> regs arg; print_string " (spill)"
  | Ireload -> regs arg; print_string " (reload)"
  | Iconst_int n -> print_string(Nativeint.to_string n)
  | Iconst_float s -> print_string s
  | Iconst_symbol s -> print_string "\""; print_string s; print_string "\""
  | Icall_ind -> print_string "call "; regs arg
  | Icall_imm lbl ->
      print_string "call \""; print_string lbl;
      print_string "\" "; regs arg
  | Itailcall_ind -> print_string "tailcall "; regs arg
  | Itailcall_imm lbl ->
      print_string "tailcall \""; print_string lbl;
      print_string "\" "; regs arg
  | Iextcall(lbl, alloc) ->
      print_string "extcall \""; print_string lbl;
      print_string "\" "; regs arg;
      if not alloc then print_string " (noalloc)"
  | Istackoffset n ->
      print_string "offset stack "; print_int n
  | Iload(chunk, addr) ->
      Printcmm.chunk chunk;
      print_string "[";
      Arch.print_addressing reg addr arg;
      print_string "]"
  | Istore(chunk, addr) ->
      Printcmm.chunk chunk;
      print_string "[";
      Arch.print_addressing reg addr (Array.sub arg 1 (Array.length arg - 1));
      print_string "] := ";
      reg arg.(0)
  | Ialloc n -> print_string "alloc "; print_int n
  | Iintop(op) -> reg arg.(0); intop op; reg arg.(1)
  | Iintop_imm(op, n) -> reg arg.(0); intop op; print_int n
  | Inegf -> print_string "-f "; reg arg.(0)
  | Iabsf -> print_string "absf "; reg arg.(0)
  | Iaddf -> reg arg.(0); print_string " +f "; reg arg.(1)
  | Isubf -> reg arg.(0); print_string " -f "; reg arg.(1)
  | Imulf -> reg arg.(0); print_string " *f "; reg arg.(1)
  | Idivf -> reg arg.(0); print_string " /f "; reg arg.(1)
  | Ifloatofint -> print_string "floatofint "; reg arg.(0)
  | Iintoffloat -> print_string "intoffloat "; reg arg.(0)
  | Ispecific op ->
      Arch.print_specific_operation reg op arg
(*e: function [[Printmach.operation]] *)

(*s: function [[Printmach.instr]] *)
let rec instr i =
  if !print_live then begin
    open_box 1;
    print_string "{";
    regsetaddr i.live;
    if Array.length i.arg > 0 then begin
      print_space(); print_string "+"; print_space(); regs i.arg
    end;
    print_string "}";
    close_box();
    print_cut()
  end;
  begin match i.desc with
    Iend -> ()
  | Iop op ->
      operation op i.arg i.res
  | Ireturn ->
      print_string "return "; regs i.arg
  | Iifthenelse(tst, ifso, ifnot) ->
      open_vbox 2;
      print_string "if "; test tst i.arg; print_string " then"; print_cut();
      instr ifso;
      begin match ifnot.desc with
        Iend -> ()
      | _ -> print_break 0 (-2); print_string "else"; print_cut(); instr ifnot
      end;
      print_break 0 (-2); print_string "endif";
      close_box()
  | Iswitch(index, cases) ->
      print_string "switch "; reg i.arg.(0);
      for i = 0 to Array.length cases - 1 do
        print_cut();
        open_vbox 2;
        open_box 0;
        for j = 0 to Array.length index - 1 do
          if index.(j) = i then begin
            print_string "case "; print_int j; print_string ":";
            print_cut()
          end
        done;
        close_box(); print_cut();
        instr cases.(i);
        close_box()
      done;
      print_cut(); print_string "endswitch"
  | Iloop(body) ->
      open_vbox 2;
      print_string "loop"; print_cut();
      instr body; print_break 0 (-2); 
      print_string "endloop ";
      close_box()
  | Icatch(body, handler) ->
      open_vbox 2;
      print_string "catch"; print_cut();
      instr body;
      print_break 0 (-2);  print_string "with"; print_cut();
      instr handler;
      print_break 0 (-2); print_string "endcatch";
      close_box()
  | Iexit ->
      print_string "exit"
  | Itrywith(body, handler) ->
      open_vbox 2;
      print_string "try"; print_cut();
      instr body;
      print_break 0 (-2);  print_string "with"; print_cut();
      instr handler;
      print_break 0 (-2); print_string "endtry";
      close_box()
  | Iraise ->
      print_string "raise "; reg i.arg.(0)
  end;
  begin match i.next.desc with
    Iend -> ()
  | _ -> print_cut(); instr i.next
  end
(*e: function [[Printmach.instr]] *)

(*s: function [[Printmach.fundecl]] *)
let fundecl f =
  open_vbox 2;
  print_string f.fun_name;
  print_string "("; regs f.fun_args; print_string ")";
  print_cut();
  instr f.fun_body;
  close_box()
(*e: function [[Printmach.fundecl]] *)

(*s: function [[Printmach.phase]] *)
let phase msg f =
  print_string "*** "; print_string msg; print_newline(); 
  fundecl f; print_newline()
(*e: function [[Printmach.phase]] *)

(*s: function [[Printmach.interference]] *)
let interference r =
  open_box 2;
  reg r; print_string ":";
  List.iter
    (fun r -> print_space(); reg r)
    r.interf;
  close_box();
  print_newline()
(*e: function [[Printmach.interference]] *)

(*s: function [[Printmach.interferences]] *)
let interferences () =
  print_string "*** Interferences"; print_newline();
  List.iter interference (Reg.all_registers())
(*e: function [[Printmach.interferences]] *)

(*s: function [[Printmach.preference]] *)
let preference r =
  open_box 2;
  reg r; print_string ": ";
  List.iter
    (fun (r, w) -> print_space(); reg r; print_string " weight " ; print_int w)
    r.prefer;
  close_box();
  print_newline()
(*e: function [[Printmach.preference]] *)

(*s: function [[Printmach.preferences]] *)
let preferences () =
  print_string "*** Preferences"; print_newline();
  List.iter preference (Reg.all_registers())
(*e: function [[Printmach.preferences]] *)
(*e: asmcomp/printmach.ml *)
