Plan: backport Custom blocks + Int32/Int64/Nativeint
====================================================

Git archaeology done against a fresh clone of the upstream ocaml/ocaml
trunk repo (blob-less clone into scratchpad, not this tree) on 2026-09-15,
to figure out exactly which commits to cherry-pick/forward-port for
`todo.org`'s "backport custom block, useful for Bignum and Int32/Int64"
and "backport Int32.ml and Int64.ml (depends on custom block)" items.

**Status as of 2026-09-15: DONE. Items #0 through #5 are all landed**
(see git log for the full commit list; each item is 2-4 commits: a
faithful cherry-pick, sometimes a `[partial]` pulled-forward fix from
a later upstream commit, and an ocaml-light-adjustment commit). Full
build/test verified after each (`make world`/`opt`/`test`/`check`,
plus the `make bootstrap` dance whenever the C primitive table
changed -- see `backport_guide.md`), plus live functional smoke tests
(not just build success) at several points. This concludes the plan:
`todo.org`'s "backport custom block, useful for Bignum and Int32/Int64"
and "backport Int32.ml and Int64.ml" items are both done. `Int32`/
`Int64` work end-to-end (arithmetic, `Marshal`, string conversion) via
`external`s over an abstract `type t`, without the literal-suffix
syntax (`3l`, `3L`) or native-unboxed-register arithmetic that the
separately-deferred `b09f44025` would add -- see that section below,
still out of scope. Nativeint was dropped entirely (see item #3's
section below) -- pick that up as a fresh, explicitly-scoped follow-up
if ever wanted, together with `utils/nativeint.ml`'s deletion and the
asmcomp repoint it requires.

See the per-item corrections below for what changed from the original
archaeology -- the item #2/#3 boundary was wrong, Nativeint was
deliberately dropped, and four more genuine upstream bugs were found
and fixed (three pulled forward as `[partial]` commits, one -- item
#5's, entangled in the deferred commit -- fixed inline).

This file was a REPORT / plan only when first written; it now also
tracks what actually happened during the port, since the real diffs
turned out to differ from the initial archaeology in a few places.

Why this matters
----------------

ocaml-light (forked from 1.07, Dec 1997) still has the pre-2000 "Final
block" mechanism: `byterun/mlvalues.h` has `Final_tag=255` / `final_fun`,
`alloc_final()` in `byterun/alloc.c`, and direct switch-on-`Final_tag` in
`compare.c`/`hash.c`/`extern.c`/`major_gc.c`/`gc_ctrl.c`. Upstream replaced
this with the more general "Custom block" mechanism (a vtable of
finalize/compare/hash/serialize/deserialize) specifically to be able to
implement `Int32.t`/`Int64.t`/`Nativeint.t` (and later Bignum) as boxed
values with custom GC/comparison/marshaling behavior. We need the same
replacement before Int32/Int64 can exist.

The chain, in order
-------------------

**0. `7175ab048dcaaa39649ebc386ae37750baaf27e1` -- "Generaliser les operations Reverse" (2000-02-10 14:03:09)**

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

**1. `9e206909f48d5d2579b6ec17764d3273df23ff08` -- "Introduction des blocs de type Custom" (2000-02-10 14:04:59)**

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

**2. `34a71202962072f30f27882498cb7e745b5dafd7` -- "Ajout de Int32.t et Int64.t (premiere etape)" (2000-02-11 12:03:31)**

The hash `todo.org` cites for "add Int32.t and Int64.t first step".

**Correction (found once actually porting this, 2026-09-15): this
commit is NOT "plumbing only" as first assessed below -- its real
diff to `byterun/ints.c` is ~445 lines and already includes the full
`int32_*`/`int64_*` arithmetic primitives (`int32_add`, `copy_int32`,
`format_int32`, `int32_of_string`, etc, and the `int64` equivalents
under the `SIZEOF_LONG == 8 || SIZEOF_LONG_LONG == 8` guard, with an
`invalid_arg` fallback stub set for platforms without it). Only the
custom_operations *registration* (`init_custom_operations`, wiring
into `startup.c`) and a few renames were left for item #3 below --
much less than originally described here.** The rest of this
subsection is the original (now superseded) archaeology, kept for
context:

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

**3. `1cac40336824df625d468405459febc63effd292` -- "Ajout des modules Int32, Int64 et Nativeint" (2000-02-11 15:09:27)**

**Missing from `todo.org`.** Landed 2026-09-15, **Nativeint excluded**
(see decision below). Its real diff is smaller than originally
guessed here (~150 lines to `ints.c`, not the primitives themselves --
those were already in item #2, see the correction above): mainly
`init_custom_operations()` (registers `int32_ops`/`int64_ops`, called
from both `byterun/startup.c` and `asmrun/startup.c`), renaming
`format_int32`/`format_int64` -> `int32_format`/`int64_format` (which,
as a happy accident, fixes a long-dangling reference in
`stdlib/printf.ml` from an unrelated earlier "frontport a more recent
printf.ml" commit), giving `int64_ops` its own `"_j"` identifier
(was sharing `"_i"` with `int32_ops`), and a `Nativeint.t` custom-block
implementation in `ints.c` plus -- important gotcha, see below --
**deleting `utils/nativeint.ml`/`.mli`** (the compiler's internal
nativeint helper), repointing `asmcomp/cmmgen.ml` and the i386
backend from `Nativeint.from`/`.shift` to the real stdlib `Nativeint.of_int`/
`.shift_left` API.

**Nativeint decision (2026-09-15): excluded from this cherry-pick.**
The stdlib `Nativeint` module this commit's asmcomp rename points at
doesn't exist yet in this fork (that's item #5, two days later
upstream) -- deleting `utils/nativeint.ml` now would break `make opt`
until item #5 lands. The original `todo.org` scope (Bignum, ogit, o5l
ELF linker) only calls for Int32/Int64 anyway, so: applied only the
Int32/Int64 half of this commit (`init_custom_operations` for
`int32_ops`/`int64_ops` only, the two renames, the `"_j"` id fix);
skipped `ints.c`'s `nativeint_ops`/`copy_nativeint`/`nativeint_*`,
`mlvalues.h`'s `Nativeint_val`, `intern.c`'s `deserialize_error` (only
used by `nativeint_deserialize`), and every `asmcomp/*.ml` rename +
the `utils/nativeint.ml`/`.mli` deletion. If Nativeint support is
wanted later, redo this commit's skipped half together with item #5's
stdlib/nativeint.ml creation, in one step, so `make opt` never breaks
in between.

**Two more bugs found while functionally testing Int64 after this
item** (both real, upstream, present in `ints.c` since item #2,
pulled forward as their own `[partial]` commits rather than fixed
in the adjustment commit, since they trace to identifiable later
upstream fixes):
- `copy_int64` allocated only 4 bytes for an 8-byte `int64`
  (`alloc_custom(&int64_ops, 4, 0, 1)`) -- heap corruption on every
  `Int64.t` allocation. Fixed by Jacques Garrigue's
  `a61816a69c1ad08ea5e1191d181ada87df55fd80` ("correct size in
  copy_int64"), 2000-02-17, 6 days later.
- `int64_serialize` had `*wsize_64 = *wsize_64 = 8;` (should assign
  `*wsize_32` first) -- left `wsize_32` uninitialized, breaking
  `Marshal`/`output_value` on any `Int64.t` ("output_value: object too
  big", reproduced live). Fixed by Xavier Leroy's
  `22b3c296c1c4773c3117951765ca18438df71816` ("Bugs dans la
  serialisation des objets custom (PR#238)"), 2000-11-30 -- 9 months
  later; an unusually large gap for a pulled-forward fix, but the bug
  and fix are both small and verified against the raw blob.

**4. `34068509c888623640b140b7aaa8299d285c21d9` -- "Revu la configuration des entiers 64 bits" (2000-02-11, same day)**

Landed 2026-09-15. Small cleanup, exactly as guessed: centralizes the
"which C type backs int64" decision into `configure` (new
`ARCH_INT64_TYPE`/`ARCH_UINT64_TYPE`/`ARCH_INT64_PRINTF_FORMAT` macros,
replacing the `SIZEOF_LONG == 8 || SIZEOF_LONG_LONG == 8` check
duplicated across `config.h`/`custom.c`/`ints.c`). `custom.c` needed
one hand adjustment for the `nativeint_ops` reference (skipped, same
Nativeint-exclusion as item #3). `config/m-nt.h` (Windows) doesn't
exist here, `config/m-templ.h` is the same dead doc file skipped
before -- both skipped.

**Found and pulled forward one more bug**: this commit's own
`configure` checks `test $1 = 8` (sizeof(int)) instead of `test $2 = 8`
(sizeof(long)) to decide whether to use plain `long` for
`ARCH_INT64_TYPE` -- since `sizeof(int)` is essentially never 8, every
64-bit host (including this one) always fell through to the "probe
`long long`" branch, getting `long long` instead of the intended
`long` (harmless functionally, both are 8 bytes, but undercuts the
whole point of this cleanup commit). Fixed 3 days later by Pierre
Weis's `2429f44a3bb5532cd89fbfdfff257ebc0f49088d` ("Mauvaise detection
de Int64.t sur archi 64 bits"), pulled forward as its own `[partial]`
commit. Confirmed live: `ARCH_INT64_TYPE` in `config/m.h` flipped from
`long long` to `long` after the fix.

**5. `15f811734e33051581406135d05ecf9769a1f031` -- "Ajout Int32, Int64 et Nativeint" (2000-02-13 16:44:06)**

Landed 2026-09-15, **Nativeint excluded** (same decision as item #3).
Adds the actual `stdlib/int32.{ml,mli}` and `int64.{ml,mli}` -- thin
`external` wrappers over the primitives from item #2, `type t`
abstract (no dependency on any compiler-recognized predefined type).
Skipped `stdlib/nativeint.{ml,mli}` entirely, matching item #3.

**Correction on Makefile placement**: the original guess above ("right
after marshal.cmo/obj.cmo, before lexing.cmo") was wrong -- upstream's
real placement (in item #3's own Makefile hunk, added there 2 days
before these files existed) appends `int32.cmo int64.cmo
nativeint.cmo` at the very *end* of `OBJS`. Matched that intent by
appending `int32.cmo int64.cmo` at the end of our own (much longer,
independently-evolved) `OBJS` list instead of the exact same neighbor
token, which doesn't appear adjacent in our list at all.

**Two more bugs found** (three, really, but two share one root cause),
same story as before -- real, upstream, confirmed against the raw
blob, but this time the actual fix is entangled inside the huge
*deferred* `b09f44025` commit (predef types + native unboxing), so
pulling forward a `[partial]` slice of it didn't make sense; fixed
directly in the adjustment commit instead:
- `stdlib/int32.ml` and `stdlib/int64.ml`: `let max = add min one`
  should be `sub min one` (`min - 1` wraps via two's-complement
  underflow to the correct max positive value; `min + 1` does not).
  Confirmed live: `Int32.max`/`Int64.max` printed deeply negative
  numbers before the fix, correct positive ones after.
- `stdlib/int64.ml`/`.mli`: `external to_int32: Int32.t -> int =
  "int64_to_int32"` -- backwards and wrong (should be `t -> Int32.t`).
  Both files agreed on the wrong signature so it type-checked, but was
  unusable as declared. Confirmed live with an `Int64.of_int32`/
  `to_int32` round-trip test, broken before the fix, correct after.

Also updated two of this fork's own Makefile variables that don't
exist in upstream at all (`Makefile`'s `PERVASIVES`,
`otherlibs/threads/Makefile`'s `LIB_OBJS` -- both explicitly flagged
by "#pad:"/"#coupling" comments as needing to stay in sync with
`stdlib/Makefile`'s `OBJS`) and regenerated `stdlib/.depend` via `make
depend`.

Explicitly skip from this time window
-------------------------------------

- `65b246b9d15b739409e027127952f63f14cd0c57` "print_flush -> print_newline
  dans le format d'affichage des warnings" -- unrelated, interleaved by
  svn revision number only.
- `1e84be9cd56b5466befa3f7b1957f7421b32f417` "label related fixes" --
  not applicable, ocaml-light does not have labels.

Deferred to a later pass (not part of this plan's scope)
--------------------------------------------------------

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

No doubled surface area (correction from initial report)
--------------------------------------------------------

An initial pass over this plan claimed `byterun/` and `asmrun/` have
separate copies of `alloc.c`/`compare.c`/`hash.c`/`extern.c`/`intern.c`/
`major_gc.c` that would each need editing twice. That's wrong:
`asmrun/{alloc,compare,extern,hash,intern,major_gc}.c` are **symlinks**
to `../byterun/*.c` in this tree (verified with `ls -la asmrun/*.c`).
Editing `byterun/` is sufficient; `asmrun/startup.c` is the only
asmrun-side file that needs its own edit (adding the
`init_custom_operations()` call from commit #3, same as
`byterun/startup.c`).

Remaining known gotchas going into implementation
-------------------------------------------------

- `Final_tag`/`Final_fun` currently appear directly in `alloc.c`,
  `compare.c`, `hash.c`, `major_gc.c`, `extern.c`, `gc_ctrl.c`,
  `mlvalues.h` -- all need the `Custom_tag`/`Custom_ops_val` rewrite from
  commit #1 (single edit each, thanks to the symlinks above).
- Our `configure` is a hand-written shell script, not autoconf, so commit
  #2's configure patch (small `if`/`case` block probing
  `config/auto-aux/longlong.c`) needs manual porting rather than a
  mechanical `git apply`/cherry-pick.
- ~~Decide up front whether to include Nativeint~~ -- **decided
  2026-09-15: no, see item #3's Nativeint decision above.** Skipped
  the `utils/nativeint.ml` deletion + `cmmgen.ml`/`i386/selection.ml`/
  `selectgen.ml` repoint entirely; that name collision (our fork's
  internal compiler helper vs. the future stdlib module of the same
  name) never arises since we never introduce the stdlib module.

Getting upstream commits (updated 2026-09-15, see backport_guide.md)
---------------------------------------------------------------------

The blob-less scratchpad clone mentioned in the original archaeology
turned out to be a dead end for actually applying anything (a
promisor remote can't lazily re-serve blobs to another local repo).
What actually works, used for every commit landed so far: fetch each
commit directly from `https://github.com/ocaml/ocaml.git` with
`git fetch --depth=2 <url> <sha>:refs/some-tag`, then tag both the
tip and its parent (`git tag some-tag-parent $(git rev-parse
refs/some-tag~1)`) for reuse across the session. See
`backport_guide.md` for the full per-file patch-application method.

All SHA1s above are full 40-hex and resolve directly against
`github.com/ocaml/ocaml`.
