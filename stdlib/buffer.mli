(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*  Pierre Weis and Xavier Leroy, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* ported from 3.12 *)


(** Extensible string buffers.

   This module implements string buffers that automatically expand
   as necessary.  It provides accumulative concatenation of strings
   in quasi-linear time (instead of quadratic time when strings are
   concatenated pairwise).
*)

type t
(** The abstract type of buffers. *)

val create : int -> t
(** [create n] returns a fresh buffer, initially empty.
   The [n] parameter is the initial size of the internal string
   that holds the buffer contents. That string is automatically
   reallocated when more than [n] characters are stored in the buffer,
   but shrinks back to [n] characters when [reset] is called.
   For best performance, [n] should be of the same order of magnitude
   as the number of characters that are expected to be stored in
   the buffer (for instance, 80 for a buffer that holds one output
   line).  Nothing bad will happen if the buffer grows beyond that
   limit, however. In doubt, take [n = 16] for instance.
   If [n] is not between 1 and {!Sys.max_string_length}, it will
   be clipped to that interval. *)

val contents : t -> string
(** Return a copy of the current contents of the buffer.
   The buffer itself is unchanged. *)

val to_bytes : t -> bytes
(** Return a copy of the current contents of the buffer.
    The buffer itself is unchanged.
    @since 4.02 *)

val sub : t -> int -> int -> string
(** [Buffer.sub b off len] returns (a copy of) the substring of the
current contents of the buffer [b] starting at offset [off] of length
[len] bytes. May raise [Invalid_argument] if out of bounds request. The
buffer itself is unaffected. *)

val blit : t -> int -> string -> int -> int -> unit
(** [Buffer.blit src srcoff dst dstoff len] copies [len] characters from
   the current contents of the buffer [src], starting at offset [srcoff]
   to string [dst], starting at character [dstoff].

   Raise [Invalid_argument] if [srcoff] and [len] do not designate a valid
   substring of [src], or if [dstoff] and [len] do not designate a valid
   substring of [dst].
   @since 3.11.2
*)

val nth : t -> int -> char
(** get the n-th character of the buffer. Raise [Invalid_argument] if
index out of bounds *)

val length : t -> int
(** Return the number of characters currently contained in the buffer. *)

val clear : t -> unit
(** Empty the buffer. *)

val reset : t -> unit
(** Empty the buffer and deallocate the internal string holding the
   buffer contents, replacing it with the initial internal string
   of length [n] that was allocated by {!Buffer.create} [n].
   For long-lived buffers that may have grown a lot, [reset] allows
   faster reclamation of the space used by the buffer. *)

val add_char : t -> char -> unit
(** [add_char b c] appends the character [c] at the end of the buffer [b]. *)

val add_string : t -> string -> unit
(** [add_string b s] appends the string [s] at the end of the buffer [b]. *)

val add_substring : t -> string -> int -> int -> unit
(** [add_substring b s ofs len] takes [len] characters from offset
   [ofs] in string [s] and appends them at the end of the buffer [b]. *)

val add_subbytes : t -> bytes -> int -> int -> unit
(** [add_subbytes b s ofs len] takes [len] characters from offset
    [ofs] in byte sequence [s] and appends them at the end of buffer [b].

    @raise Invalid_argument if [ofs] and [len] do not designate a valid
    range of [s].

    @since 4.02 *)

val add_substitute : t -> (string -> string) -> string -> unit
(** [add_substitute b f s] appends the string pattern [s] at the end
   of the buffer [b] with substitution.
   The substitution process looks for variables into
   the pattern and substitutes each variable name by its value, as
   obtained by applying the mapping [f] to the variable name. Inside the
   string pattern, a variable name immediately follows a non-escaped
   [$] character and is one of the following:
   - a non empty sequence of alphanumeric or [_] characters,
   - an arbitrary sequence of characters enclosed by a pair of
   matching parentheses or curly brackets.
   An escaped [$] character is a [$] that immediately follows a backslash
   character; it then stands for a plain [$].
   Raise [Not_found] if the closing character of a parenthesized variable
   cannot be found. *)

val add_buffer : t -> t -> unit
(** [add_buffer b1 b2] appends the current contents of buffer [b2]
   at the end of buffer [b1].  [b2] is not modified. *)

val add_channel : t -> in_channel -> int -> unit
(** [add_channel b ic n] reads exactly [n] character from the
   input channel [ic] and stores them at the end of buffer [b].
   Raise [End_of_file] if the channel contains fewer than [n]
   characters. *)

val output_buffer : out_channel -> t -> unit
(** [output_buffer oc b] writes the current contents of buffer [b]
   on the output channel [oc]. *)
