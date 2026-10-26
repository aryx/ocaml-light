(* partial port of 4.02 bytes.ml just enough to compile some of ocaml-light with
 * dune and a recent OCaml (as well as xix)
 *)
type t = string

let empty = ""

let sub x n1 n2 = String.sub x n1 n2

let sub_string x n1 n2 = String.sub x n1 n2

(* claude: the primitives themselves, as String's are, not functions
 * that call them: a kernel's pixels are bytes (ix's mini-9pi), read and
 * set one at a time. unsafe_set was missing.
 * old: let length x = String.length x; let get x n = String.get x n;
 *      let set s x c = String.set s x c;
 *      let unsafe_get x n = String.unsafe_get x n *)
external length : string -> int = "%string_length"

let create x = String.create x

let to_string x = x

let of_string x = x

external get : string -> int -> char = "%string_safe_get"

external set : string -> int -> char -> unit = "%string_safe_set"

let index_from s n c = String.index_from s n c

external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : string -> int -> char -> unit = "%string_unsafe_set"

let unsafe_to_string x = x

let unsafe_of_string x = x

let blit src srcoff dst dstoff len =
  String.blit src srcoff dst dstoff len

let unsafe_blit src srcoff dst dstoff len =
  String.unsafe_blit src srcoff dst dstoff len

let blit_string = blit

let fill s start len c = String.fill s start len c


let make = String.make

let equal = ( = )

let compare = compare
