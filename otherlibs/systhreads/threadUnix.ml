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


(* Module [ThreadUnix]: thread-compatible system calls *)

open Unix

(*** Process handling *)

external execv : string -> string array -> unit = "unix_execv"
external execve : string -> string array -> string array -> unit
           = "unix_execve"
external execvp : string -> string array -> unit = "unix_execvp"
let wait = Unix.wait
let waitpid = Unix.waitpid
let system = Unix.system
let read = Unix.read
let write = Unix.write

let timed_read fd buff ofs len timeout =
  if Thread.wait_timed_read fd timeout
  then Unix.read fd buff ofs len
  else raise (Unix_error(ETIMEDOUT, "timed_read", ""))

let timed_write fd buff ofs len timeout =
  if Thread.wait_timed_write fd timeout
  then Unix.write fd buff ofs len
  else raise (Unix_error(ETIMEDOUT, "timed_write", ""))

let pipe = Unix.pipe

let open_process_in = Unix.open_process_in
let open_process_out = Unix.open_process_out
let open_process = Unix.open_process

external sleep : int -> unit = "unix_sleep"

let socket = Unix.socket
let accept = Unix.accept
external connect : file_descr -> sockaddr -> unit = "unix_connect"
let recv = Unix.recv
let recvfrom = Unix.recvfrom
let send = Unix.send
let sendto = Unix.sendto

let open_connection = Unix.open_connection
