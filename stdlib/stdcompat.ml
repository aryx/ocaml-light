(* When using ocaml-light, we can't also use dune and install the
 * 'stdcompat' package so we provide a poor's man stdcompat
 * that at least will allow to compile the xix code relying
 * on stdcompat.
 *)

module Stdlib = struct
  let compare = Pervasives.compare
end
