(*s: asmcomp/emitaux.ml *)
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

(*s: constant Emitaux.output_channel *)
(* Common functions for emitting assembly code *)

let output_channel = ref stdout
(*e: constant Emitaux.output_channel *)

(*s: function Emitaux.emit_string *)
let emit_string s = output_string !output_channel s
(*e: function Emitaux.emit_string *)

(*s: function Emitaux.emit_int *)
let emit_int n = output_string !output_channel (string_of_int n)
(*e: function Emitaux.emit_int *)

(*s: function Emitaux.emit_char *)
let emit_char c = output_char !output_channel c
(*e: function Emitaux.emit_char *)

(*s: function Emitaux.emit_nativeint *)
let emit_nativeint n = output_string !output_channel (Nativeint.to_string n)
(*e: function Emitaux.emit_nativeint *)

(*s: function Emitaux.emit_printf *)
(* @Scheck: used by mips backend *)
let emit_printf fmt =
  Printf.fprintf !output_channel fmt
(*e: function Emitaux.emit_printf *)

(*s: function Emitaux.emit_symbol *)
let emit_symbol esc s =
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
      'A'..'Z' | 'a'..'z' | '0'..'9' | '_' ->
        output_char !output_channel c
    | _ ->
        Printf.fprintf !output_channel "%c%02x" esc (Char.code c)
  done
(*e: function Emitaux.emit_symbol *)

(*s: function Emitaux.emit_string_literal *)
let emit_string_literal s =
  let last_was_escape = ref false in
  emit_string "\"";
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c >= '0' && c <= '9' then
      if !last_was_escape
      then Printf.fprintf !output_channel "\\%o" (Char.code c)
      else output_char !output_channel c
    else if c >= ' ' && c <= '~' && c <> '"' (* '"' *) && c <> '\\' then begin
      output_char !output_channel c;
      last_was_escape := false
    end else begin
      Printf.fprintf !output_channel "\\%o" (Char.code c);
      last_was_escape := true
    end
  done;
  emit_string "\""
(*e: function Emitaux.emit_string_literal *)

(*s: function Emitaux.emit_string_directive *)
let emit_string_directive directive s =
  let l = String.length s in
  if l = 0 then ()
  else if l < 80 then begin
    emit_string directive;
    emit_string_literal s;
    emit_char '\n'
  end else begin
    let i = ref 0 in
    while !i < l do
      let n = min (l - !i) 80 in
      emit_string directive;
      emit_string_literal (String.sub s !i n);
      emit_char '\n';
      i := !i + n
    done
  end
(*e: function Emitaux.emit_string_directive *)

(*s: function Emitaux.emit_bytes_directive *)
let emit_bytes_directive directive s =
   let pos = ref 0 in
   for i = 0 to String.length s - 1 do
     if !pos = 0
     then emit_string directive
     else emit_char ',';
     emit_int(Char.code s.[i]);
     incr pos;
     if !pos >= 16 then begin emit_char '\n'; pos := 0 end
   done;
   if !pos > 0 then emit_char '\n'
(*e: function Emitaux.emit_bytes_directive *)

(*e: asmcomp/emitaux.ml *)
