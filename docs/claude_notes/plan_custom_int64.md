# Plan: backport Custom blocks + Int32/Int64/Nativeint

Git archaeology done against a fresh clone of the upstream ocaml/ocaml
trunk repo (blob-less clone into scratchpad, not this tree) on 2026-09-15,
to figure out exactly which commits to cherry-pick/forward-port for
`todo.org`'s "backport custom block, useful for Bignum and Int32/Int64"
and "backport Int32.ml and Int64.ml (depends on custom block)" items.

This is a REPORT / plan only -- nothing has been ported yet.

## Why this matters

ocaml-light (forked from 1.07, Dec 1997) still has the pre-2000 "Final
block" mechanism: `byterun/mlvalues.h` has `Final_tag=255` / `final_fun`,
`alloc_final()` in `byterun/alloc.c`, and direct switch-on-`Final_tag` in
`compare.c`/`hash.c`/`extern.c`/`major_gc.c`/`gc_ctrl.c`. Upstream replaced
this with the more general "Custom block" mechanism (a vtable of
finalize/compare/hash/serialize/deserialize) specifically to be able to
implement `Int32.t`/`Int64.t`/`Nativeint.t` (and later Bignum) as boxed
values with custom GC/comparison/marshaling behavior. We need the same
replacement before Int32/Int64 can exist.

## The chain, in order

### 0. `7175ab048dcaaa39649ebc386ae37750baaf27e1` -- "Generaliser les operations Reverse" (2000-02-10 14:03:09)

NOT mentioned anywhere in `todo.org`/`pad.org`. Discovered by diffing:
the custom-blocks commit's `extern.c`/`intern.c` already call the new
macro names. Rewrites `byterun/reverse.h` from in-place 1-arg macros
(`Reverse_int32(w)`, `Reverse_double(d)`) to 2-arg dst/src macros
(`Reverse_16/32/64(dst,src)`). Same author, one minute before the
custom-blocks commit, consecutive svn revisions (2803 -> 2804) --
clearly a direct prerequisite, not a coincidence.

Our current call sites needing the rename: `byterun/fix_code.c` (1),
`byterun/intern.c` (2). `asmrun/*.c` are symlinks to `byterun/*.c` in
this tree, so that's the complete list (see the symlinks note below).

### 1. `9e206909f48d5d2579b6ec17764d3273df23ff08` -- "Introduction des blocs de type Custom" (2000-02-10 14:04:59)

The commit `todo.org` already flagged ("(BIG) introduction of Custom
blocs"). Adds `byterun/custom.c` + `custom.h` (the `struct
custom_operations` vtable: finalize/compare/hash/serialize/deserialize +
registration table: `register_custom_operations`/`find_custom_operations`).
Renames `Final_tag` -> `Custom_tag` in `mlvalues.h` (`Data_custom_val(v)`
replaces the raw `Field(v,0)` access). Updates every consumer:

- `alloc.c`: `alloc_final()` rewritten on top of new `alloc_custom()`
- `compare.c`, `hash.c`, `extern.c`: switch on `Custom_tag`, dispatch to
  the vtable instead of failing/ignoring like `Final_tag` did
- `gc_ctrl.c`, `major_gc.c`: sweep calls the vtable's finalize function
- `io.c`: channels move from `alloc_final()`+`Field(v,1)` to
  `alloc_custom(&channel_operations, ...)`+`Data_custom_val`

Verified our current `alloc.c`/`alloc_final` already matches the exact
pre-commit shape shown in this diff (same `adjust_gc_speed`/
`check_urgent_gc` calls) -- clean base to apply against.

### 2. `34a71202962072f30f27882498cb7e745b5dafd7` -- "Ajout de Int32.t et Int64.t (premiere etape)" (2000-02-11 12:03:31)

The hash `todo.org` cites for "add Int32.t and Int64.t first step".
Runtime plumbing only, no arithmetic primitives yet:

- moves `alloc_custom` into `custom.c` properly
- `byterun/config.h`: real `int32`/`uint32`/`int64`/`uint64` typedefs
  driven by `SIZEOF_INT`/`SIZEOF_LONG`/`SIZEOF_LONG_LONG` (replaces the
  `typedef long int64; /* FIXME */` placeholder that commit #1
  introduced)
- `config/auto-aux/longlong.c` (new): probe program to detect a working
  `long long`
- `config/m-templ.h` + `configure`: wire up the `SIZEOF_LONG_LONG`
  detection (this is the "need first SIZEOF_INT == ..." note in
  `todo.org`) -- our configure is a hand-written shell script, not
  autoconf, so this needs manual porting, but the upstream patch itself
  is a small self-contained `if`/`case` block
- `mlvalues.h`: `Int32_val(v)`/`Int64_val(v)` accessors on `Data_custom_val`

### 3. `1cac40336824df625d468405459febc63effd292` -- "Ajout des modules Int32, Int64 et Nativeint" (2000-02-11 15:09:27)

**Missing from `todo.org`** -- this is the commit that actually implements
the C primitives (`int32_add`/`_sub`/`_of_string`/... in `byterun/ints.c`,
~150 new lines), registers them via `init_custom_operations()` (called
from both `byterun/startup.c` and `asmrun/startup.c`), and -- important
gotcha below -- **deletes `utils/nativeint.ml`/`.mli`** (the compiler's
internal nativeint helper), repointing `asmcomp/cmmgen.ml` and the i386
backend from `Nativeint.from`/`.shift` to the real stdlib `Nativeint.of_int`/
`.shift_left` API.

### 4. `34068509c888623640b140b7aaa8299d285c21d9` -- "Revu la configuration des entiers 64 bits" (2000-02-11, same day)

Small follow-up refining the 64-bit config detection from #2/#3. Not yet
inspected in detail -- check when implementing, expected small.

### 5. `15f811734e33051581406135d05ecf9769a1f031` -- "Ajout Int32, Int64 et Nativeint" (2000-02-13 16:44:06)

Adds the actual `stdlib/int32.{ml,mli}`, `int64.{ml,mli}`,
`nativeint.{ml,mli}` -- thin wrappers over the `external` primitives from
#3. This is the file `todo.org`'s other link (github commit
`15f811734...`) pointed at, confirmed correct.

Needs adding to `stdlib/Makefile` build order: goes right after
`marshal.cmo`/`obj.cmo`, before `lexing.cmo` (matches upstream's own
ordering, see the `stdlib.cma` link line in any post-3.00 build trace).

## Explicitly skip from this time window

- `65b246b9d15b739409e027127952f63f14cd0c57` "print_flush -> print_newline
  dans le format d'affichage des warnings" -- unrelated, interleaved by
  svn revision number only.
- `1e84be9cd56b5466befa3f7b1957f7421b32f417` "label related fixes" --
  not applicable, ocaml-light does not have labels.

## Deferred to a later pass (not part of this plan's scope)

`b09f44025c213498435690ce22c21c1b156e2def` -- "Ajout des types predefinis
int32, int64, nativeint. Ajout des primitives correspondantes dans le type
lambda. Optimisation de ces primitives dans le compilateur natif"
(2000-02-21 18:14:56, 10 days after #5).

This is a second, separate, much heavier wave:

- `typing/predef.ml`: makes `int32`/`int64`/`nativeint` compiler-recognized
  predefined types -- this is what enables literal suffix syntax
  (`3l : int32`, `3L : int64`, `3n : nativeint`), which needs lexer +
  parser + parsetree changes we don't have at all yet
- `bytecomp/lambda.ml`, `translcore.ml`, `bytegen.ml`: unboxes these
  primitives into dedicated lambda primitives instead of going through
  the generic external-C-call path
- `asmcomp/cmmgen.ml`: 634-line diff, native-code register-level unboxing
  of int32/int64/nativeint arithmetic

Chain #0-#5 above already gives a fully correct, usable Int32/Int64/
Nativeint: arithmetic goes through ordinary boxed custom-block C
primitive calls (via plain `external` declarations, using infrastructure
we already have for every other stdlib primitive) rather than through
unboxed machine registers. Given the stated motivation (Bignum, ogit, o5l
ELF linker -- reading/writing fixed-width ints) is about
availability/correctness, not hot-loop arithmetic performance, cutting
the MVP here and deferring `b09f44025` looks like the right call. Revisit
only if profiling ever shows boxing overhead matters, and note it would
need porting across every asmcomp backend we support (i386/amd64/arm/
arm64), not just one.

## No doubled surface area (correction from initial report)

An initial pass over this plan claimed `byterun/` and `asmrun/` have
separate copies of `alloc.c`/`compare.c`/`hash.c`/`extern.c`/`intern.c`/
`major_gc.c` that would each need editing twice. That's wrong:
`asmrun/{alloc,compare,extern,hash,intern,major_gc}.c` are **symlinks**
to `../byterun/*.c` in this tree (verified with `ls -la asmrun/*.c`).
Editing `byterun/` is sufficient; `asmrun/startup.c` is the only
asmrun-side file that needs its own edit (adding the
`init_custom_operations()` call from commit #3, same as
`byterun/startup.c`).

## Remaining known gotchas going into implementation

- `Final_tag`/`Final_fun` currently appear directly in `alloc.c`,
  `compare.c`, `hash.c`, `major_gc.c`, `extern.c`, `gc_ctrl.c`,
  `mlvalues.h` -- all need the `Custom_tag`/`Custom_ops_val` rewrite from
  commit #1 (single edit each, thanks to the symlinks above).
- Our `configure` is a hand-written shell script, not autoconf, so commit
  #2's configure patch (small `if`/`case` block probing
  `config/auto-aux/longlong.c`) needs manual porting rather than a
  mechanical `git apply`/cherry-pick.
- Decide up front whether to include Nativeint (see the gotcha under #3:
  our `asmcomp/cmmgen.ml` still uses the old internal `Nativeint.from`/
  `.shift` API from `utils/nativeint.ml`, unrelated to the future stdlib
  module of the same name -- confirmed untouched since the fork. If we
  backport Nativeint too, we hit the same name collision Xavier Leroy
  did and need the same fix: delete `utils/nativeint.ml`, repoint the
  ~4 call sites in `cmmgen.ml`/`i386/selection.ml`/`selectgen.ml`. If we
  skip stdlib `Nativeint` and only do `Int32`/`Int64`, this collision
  doesn't arise at all -- worth deciding explicitly rather than copying
  upstream blindly).

## Upstream clone location (for the actual porting session)

A blob-less clone of `github.com/ocaml/ocaml` lives in this session's
scratchpad (ephemeral -- re-clone if starting a fresh session):

```
/tmp/claude-*/-home-pad-github-ocaml-light/*/scratchpad/upstream-ocaml
```

All SHA1s above are full 40-hex and resolve directly in that clone (or
in `github.com/ocaml/ocaml`).
