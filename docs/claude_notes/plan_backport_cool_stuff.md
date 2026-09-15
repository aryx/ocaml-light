Backport/forward-port candidates: cool, light-footprint developer-experience wins
==================================================================================

Scope and method
-----------------

This is a survey of `pad.org` (the "Backport list" + "Important commits in
OCaml history" sections) and `todo.org` (scattered across "Last1"/"Last0",
"Components", "Backports"), re-read in full, plus spot checks against the
current ocaml-light tree (`stdlib/printexc.mli`, `typing/parmatch.ml`,
`parsing/parser.mly`, etc.) and `changes.txt`'s own release entries, to see
what's actually already there vs. only half-done vs. genuinely missing.
`changes.txt` in particular already records several things `todo.org`
still lists as open (record shorthand, `M.{ }` sugar, field-projection
disambiguation) -- checked each against the code directly rather than
trusting either file's bookkeeping alone.

Explicitly out of scope, on request:

- **Custom blocks / Int32 / Int64 / Nativeint** -- has its own file,
  `plan_custom_int64.md`. Not repeated here.
- Architecture ports (amd64/arm64/riscv) -- separate topic, tracked in
  `notes_asmcomp_arch_history.txt` and largely done already.

Ranking criterion, per the author's own framing: **stays "light"** (doesn't
expand the codebase or add generic infrastructure it doesn't need) **and**
is genuinely nice for day-to-day OCaml development (the author's own
example: "good backtrace"). A feature that's individually cool but would
require a large generic subsystem (a full numbered-warnings framework, a
typeclass/implicit resolution engine, a custom preprocessor) is flagged as
excluded even when upstream OCaml or `todo.org` clearly likes it, because it
fails the footprint half of the test.

Four tiers below, S/A/B/C ranked highest to lowest value-for-footprint
(gaming-tier-list convention: S for "Super", ranked above A).

Tier S (Super) -- do these first (best DX-per-line-changed)
-------------------------------------------------------------

**1. Finish native-code backtraces**

Bytecode already has real backtraces (`byterun/backtrace.c`, wired into
`stdlib/printexc.mli`'s `get_backtrace`/`raw_backtrace_to_string`). Native
code does not: `asmrun/backtrace.c` is a symlink to the *same* file, but
that mechanism is fundamentally a bytecode-PC-log design, not a real
frame-descriptor stack walk -- ocamlopt-compiled programs still crash
today with no backtrace. This is upstream's `4b5512c74cda289652ff53c01098c53a6c4b0c7a`
"(BIG) backtrace in native code" (Jan 2007), already flagged (unmarked =
open) in `pad.org`'s own curated commit list. Biggest single DX win on this
whole list -- exactly the author's own example of a "cool" feature.
Real footprint: frame descriptor tables already exist for GC purposes in
`asmrun/roots.c` (`frame_descriptor`/`stack_walk`); backtrace capture
reuses that walk instead of adding a second one. Not trivial, but scoped
to `asmrun/backtrace.c`, `asmrun/roots.c`, and the per-arch `emit.mlp`
frame-table emission -- no new stdlib module, no new syntax.

**2. Make `Printexc.raw_backtrace`/`get_callstack` real**

Checked `stdlib/printexc.ml`: today `get_raw_backtrace () = get_backtrace ()`
and `get_callstack _n = get_backtrace ()` -- both are stub aliases onto the
*string*-based backtrace, not the real deferred/structured representation
upstream has since 4.01/4.05. `todo.org` flags this directly: "backport
Printexc.raw_backtrace so can get working Exception.ml" (needed by xix).
Small, self-contained fix once #1 exists (the abstract `raw_backtrace`
type already exists in the `.mli` -- just needs a real non-string backing
representation instead of the current string alias).

**3. Show the actual missing case(s) in "not exhaustive" errors**

Checked `typing/parmatch.ml:297`: today it's a hardcoded
`Location.print_warning loc "this pattern-matching is not exhaustive"`
with zero example of what's missing -- exactly `todo.org`'s own complaint
("with no explanations of the cases..."). This is upstream OCaml 2.03
(1999): the first version to report an actual counter-example value
("Here is an example of a case that is not matched"). Needs a small
witness-pattern generator in the match compiler, not the general
Warnings-numbering framework -- self-contained to `typing`/`bytecomp`'s
matching code, gets 90% of the value.

**4. Name the actual label(s) in "Some labels are undefined"**

Same shape of fix as #3, in `typecore.ml` this time. `todo.org` quotes the
exact unhelpful message and asks for at least one label name, ideally all
of them. The record-fields-provided vs. label-set-expected data is already
on hand at the error site; this is a "compute plus print" fix, not new
infrastructure.

**5. Warn when a sequenced expression's result isn't `unit`**

`todo.org`'s own bug story: `Event.send ctl.chan buf.[i]` silently returned
non-unit and ocaml-light said nothing about it being dropped in sequence
position. Turns out this is a very old, very small, already-proven upstream
feature: OCaml 2.01 (1998) added exactly this warning, specifically to
catch things like `record.lbl = newval; ...` typoed instead of `<-`. Same
"one targeted diagnostic, no framework" shape as #3/#4 -- check the
discarded-expression's type at `;`-sequence sites in `typecore.ml`/
`translcore.ml`. Promoted into this tier since it's both old and small
enough that upstream shipped it as a first-cut warning, not a whole
subsystem.

Tier A -- small syntax/typer sugar, self-contained
-----------------------------------------------------

**6. Propagate a qualified field's module to its unqualified siblings**

`todo.org`'s remaining piece of "more type-directed disambiguation" --
checked, and it's genuinely still open, unlike its sibling features (see
"Already effectively done" below). Today, in the *plain*, un-prefixed
record-literal style:

```ocaml
let conf : Preprocessor.conf = { Preprocessor.
    defs = !macro_defs;
    paths = system_paths @ List.rev !include_paths;
    dir_source_file = Fpath.v (Filename.dirname !infile);
  }
```

`typecore.ml`'s `Pexp_record` case (around line 326-346) resolves every
label independently via a bare `Env.lookup_label lid env` with *no*
type-directed fallback at all -- unlike `Pexp_field`/`Pexp_setfield` a few
cases below it, which already do exactly this kind of fallback (try the
plain label, then retry qualified by the known record type, see
`typecore.ml:370-414`). The fix is to give record-literal typing the same
fallback its two neighbors already have: if the first field is qualified
(`Preprocessor.defs`), or the expected type is already known from an
annotation, use that to resolve the rest. Small, typer-local, and
directly reduces the `Module.` qualifier noise the style guide already
tries to avoid (`todo.org`: "just cherry pick the patch that did that in
original ocaml"). Note this is a different, narrower feature than the
already-implemented `M.{ field = ...; ... }` prefix sugar -- see below.

**7. Unused-variable warning, with `_`-prefix to silence it**

`todo.org` quotes `Warning 26: unused variable t.` as something to
backport, paired with the existing `_foo` convention to opt out. Upstream
shipped exactly this (as warnings `'Y'`/`'Z'`, later renumbered) in 3.09.0
-- confirmed genuinely small there too: a use-count pass over `Ident.t`
bindings, not the general `-w` framework. Does not require the full
numbered Warnings module (see "excluded" below) -- a single targeted check
in `typecore.ml`/`env.ml` is enough.

**8. `let*` / `let+` / `and*` binding operator syntax**

OCaml 4.08.0, 2019 (PR#1947, "Allow custom `let` operators"). Pure parser
+ typer sugar (`let* x = e in ...` desugars to `( let* ) e (fun x -> ...)`)
-- no ppx, no attributes, nothing from the excluded list. Very popular for
`Result`/`Option`-style railway-oriented code, which is exactly the style
ocaml-light's own "records over first-class modules/functors" philosophy
already leans toward. Self-contained: a handful of new operator-name
lexer/parser cases plus their desugaring in `parsing`/`typing`, no new
runtime or stdlib requirement (users define their own `( let* )`/
`( and* )` per-monad, same as upstream). Arguably the single best
value-for-footprint syntax feature on this whole list.

**9. `Seq` module**

OCaml 4.07 (2018), grown steadily through 4.14. A dependency-free
lazy-sequence stdlib module (`type 'a t = unit -> 'a node`) -- pure OCaml,
zero runtime changes, no interaction with anything on the excluded list.
Genuinely useful (lazy `Hashtbl`/`Map`/`Set` iteration, composable
generators). Don't port all 40+ later functions at once -- start with the
minimal core (`empty`, `cons`, `map`, `filter`, `fold_left`,
`of_list`/`to_list`), which is about as "light" as a stdlib addition gets,
and grow it only as real call sites want more.

**10. Small lexer-only catches**

Three tiny, independent, well-proven-upstream lexer changes, each a
handful of lines in `parsing/lexer.mll` with no parser/typer impact:
  - **`1_000_000`-style underscore separators in int/float literals**
    (OCaml 3.05, 2002) -- near-zero footprint, an extremely well-known and
    missed QoL win for anyone pasting in a large constant.
  - **Warning for `(*)`/`*)` appearing outside a comment** (2.03, 1999) --
    catches a real, common typo.
  - **Out-of-range escape detection, e.g. `"\256"`** (3.04, 2001) -- a
    small bounds check that turns a silent wraparound bug into a compile
    error.

**11. `Match_failure`/`Assert_failure` report `(file, line, column)`**

OCaml 3.07 (2003). Today these exceptions stringify as
`(file, start_char, end_char)` -- character offsets into the file, which
nobody can place at a glance. Upstream's change is purely how the
exception's payload gets formatted when printed; much more readable when
one of these actually fires during debugging. Tiny, self-contained.

Tier B -- smaller/medium value, still light
-----------------------------------------------

**12. Unicode `\u{X+}` string escape**

OCaml 2017, `dae520ca1275d6d38e12f63ce508065cc0c5471a`. Lexer-only change
(one new escape-sequence case in `parsing/lexer.mll`), already flagged
open (no DONE/SEMI marker) in `pad.org`'s curated list.

**13. `Unix.realpath`**

`pad.org` flags `62b946efae7fa2aa3859b81e9cd8922d3f0c4134` (2020);
`todo.org` independently lists "Unix.realpath!" too. `todo.org` also notes
the real blocker isn't the function itself but that "primitives are hard!
need to promote, so better wait we have a few of them" -- i.e. batch this
with #14 and any other small missing `Unix`/`Sys` primitive so the
promotion-machinery cost is paid once for several wins.

**14. Verified-missing small stdlib gap-fillers**

Checked each of these against the current `.mli` files rather than
trusting version history alone -- ocaml-light's stdlib has already grown
a lot past vanilla 1.07 (`List.iteri`, `mem_assoc`, `filter_map`,
`String.starts_with`/`ends_with`, `Stack.top`/`top_opt`, `Hashtbl.fold`/
`mem`, and `Arg.align` are already there, so skip those). Genuinely
missing and each a few self-contained lines:
  - `List.rev_append`, `mem_assq`, `remove_assoc`, `remove_assq`,
    `compare_lengths`, `compare_length_with`, `equal`, `compare`
  - `String.contains`, `contains_from`, `rcontains_from`, `fold_left`,
    `fold_right`, `exists`, `for_all` (note: there is no `Bytes` module in
    this tree at all -- strings are still mutable, pre-safe-string-split,
    as expected for a 1.07-era fork -- so these land on `String` directly)
  - `Array.exists2`, `for_all2`, `find_opt`, `find_map`, `fold_left_map`
  - `Pervasives.max_float`, `min_float`, `epsilon_float`, `flush_all`
  - `Sys.is_directory`, `Sys.executable_name`
  - `Lazy.map`/`map_val` -- worth noting `Lazy` here is extremely bare
    today (only `force` exists, not even `is_val`/`from_val`/`from_fun`),
    so this is really "flesh out `Lazy` a bit" rather than one function

**15. Small runtime debug-mode helpers**

Three small Damien Doligez commits `todo.org` flags under "include diffs
that help the debuggability of ocaml": `5674cf35c8d59cd19bb93a39542dfad1e7d9ac9e`
("ajout heap_check"), `bad71c148081a820604b9901300a5b8e2b730a95` ("ajout
heap_check en mode debug"), `a843096a997d0a2914b8cbabd952e4a230d07598`
("codes pour faciliter le debug"), all ~1999-2000. Not user-facing
features -- small aids for whoever is hacking on the GC/runtime itself,
i.e. useful precisely when doing the rest of the work on this list.

**16. `ocamldep` rewritten to use the real parser**

`b81eec604036157120e5d622e0e6410d49fbf61c` (Jan 1999). `todo.org` traces a
real bug to this: today's regex/heuristic-based `ocamldep` gets nested
module dependencies wrong (a concrete case involving `Cap.cmo`,
`Console.cmi`, `FS.cmi`, `Process.cmo` is described first-hand). Bounded
to one tool.

Tier C -- lower priority / more debatable
----------------------------------------------

**17. Or-pattern variable bindings** (`[t] | [_;t] -> ... t ...`, OCaml
3.01) -- the author is explicitly ambivalent in `pad.org`'s own
NOT-BACKPORTED section: "convenient but better rewrite the type usually
and factorize? Useful though for `Cast (t,e) | GccConstructor (t, e) -> ...`".
Not a clean broad win; only worth it if a concrete recurring pattern in
this codebase actually wants it.

**18. `ocamllex` support for named submatches (`as`)** -- `todo.org` flags
this as wanted for the rest of `xix` (porting `mk`/`rc`'s lexers) but
rates it "too complex? diff too big?" ("LONG"). Upstream's actual feature
(3.07, 2003) may be smaller than feared in isolation, but the *use case*
here is porting substantial pre-existing `xix` lexer files against it,
which is where the real size risk lives -- worth re-estimating directly
against a concrete `xix` lexer before committing, rather than trusting
either the optimistic or pessimistic guess.

**19. Better `dynlink`** ([ocaml/ocaml#1063](https://github.com/ocaml/ocaml/pull/1063),
per `todo.org`) -- `dynlink` internals are already one of the gnarlier
corners of the compiler; likely more invasive than it looks from the
outside.

**20. ANSI-C / modern-gcc runtime cleanup** ([ocaml/ocaml#11764](https://github.com/ocaml/ocaml/pull/11764),
plus the 4.14.2 runtime fixes `todo.org` points at) -- not a "feature" so
much as portability maintenance (old-style C prototypes choke under
stricter modern `gcc`/`clang`). Worth doing eventually for "still builds
cleanly on today's toolchains", but it's chores, not coolness.

Excluded -- cool on paper, fails the "light" bar
-----------------------------------------------------

- **Typeclasses / implicits** (`todo.org` "FUN add typeclasses!") -- would
  need a resolution engine; explicitly aspirational ("FUN"), not scoped
  light. Motivating use case (dedup `Subst.value_description` /
  `Subst.type_declaration`-style repeated names) is real, but the
  mechanism to fix it generically is exactly the kind of module-system
  machinery `pad.org`'s own NOT-BACKPORTED-ON-PURPOSE list already rejects
  (functors, first-class modules) for being "too often abused when a
  record would work."
- **`deriving` / Template Haskell-style codegen** (`todo.org` "FUN add
  deriving!") -- same shape of problem: real motivating pain
  (`Subst.type_expr`-style visitor boilerplate) but the honest fix is a
  generic derivation mechanism, which is a new subsystem, not a light
  patch.
- **Custom mixfix/Unicode preprocessor a la Agda** (`todo.org`) -- a whole
  new parser generation layer; explicitly "need update also efuns and
  codemap" in the author's own note, i.e. known to ripple far outside this
  repo.
- **Attributes / extension points** (`152255e5129d94325f3017bc93623dde2fa1cfb1`,
  2013) -- already marked `NOPE` in `pad.org`'s curated list.
- **A full numbered `-w` Warnings module** -- upstream's `utils/warnings.ml`
  is generic machinery for dozens of warning classes with per-warning
  enable/disable syntax. Items #3, #4, #5, #7 above get most of the real
  value as single targeted diagnostics without needing this framework at
  all; only revisit if the number of hand-rolled diagnostics grows enough
  that the framework starts paying for itself.
- **`In_channel`/`Out_channel` modules** (4.14) -- nice, but a sizeable
  surface (many functions) for what they'd add over the existing
  `Pervasives` channel functions. Only worth it if full stdlib parity with
  modern OCaml becomes a goal in itself, which isn't the ask here.
- Everything already in `pad.org`'s NOT-BACKPORTED-ON-PURPOSE list:
  objects, functors, first-class modules, `include`, labels, open
  variants, GADTs, camlp4, deprecated archs, Windows/macOS-specific code,
  autoconf, `-I +xxx`. Not re-litigated here.
- Safe-string-scale, multicore/effects, flambda-era changes -- explicitly
  out of era for a 1.07-shaped fork; not evaluated feature-by-feature.

Already effectively done (verified against the tree, not just notes)
--------------------------------------------------------------------------

Worth calling out explicitly, since `changes.txt`'s own `* 0.3` entry
records these and it's easy to mistake them for open items from `todo.org`
alone: the `{ lbl }` / `{ M.lbl }` record shorthand is done, in **both**
expressions and patterns (`parsing/parser.mly:733-735` for patterns,
`:622-624` for expressions) -- `changes.txt` marks it "(partial)" only
because the *separate*, already-rejected `{ lbl = pat; _ }` not-all-labels
pattern marker from the same 3.12.0 release was deliberately left out, not
because the punning itself is incomplete. The `M.{ field = ...; ... }`
prefix-sugar is also done (`parser.mly:547-561`, unsugars to
`{ M.field = ...; ... }`), and `x.field`/`x.field <- v` type-directed
disambiguation is done for plain field projection/assignment
(`typecore.ml:364-391` and `:393-414+`, matching the global CLAUDE.md
house style). What's *not* done, despite living under the same
"disambiguation" heading in `todo.org`, is the record-*construction* case
-- see #6 above.

Cross-checking `pad.org`'s `!PARTIAL!`/`DONE` annotations *and* a direct
`.mli` read against the actual code, to avoid re-suggesting anything
already there: `Printexc.get_backtrace` (bytecode), `raw_backtrace`'s
*type* (if not its real semantics, see #2), `result` type, `Uchar`,
`Float`/`Option`/`Result`/`Int`/`Bool`/`Fun` modules, `Hashtbl.replace`
(partial), `Hashtbl.fold`/`mem`, `List.sort`/`stable_sort` (partial),
`List.iteri`, `mem_assoc`, `filter_map`, `ignore`, `Buffer`,
`List.find`/`filter`/`partition`, `Str.split_delim`/`full_split`,
`String.starts_with`/`ends_with`, `Stack.top`/`top_opt`, `Arg.align`,
`Arg.Set_string`/`Bool`, `Pervasives` numeric/char conversions,
`CAMLparam`/`CAMLlocal` C macros, `Unix` time-as-float,
`Str.quote`/`regexp_string`, symbolic block tags (`Obj.string_tag`),
`Printf`'s `%$`/`%!`, exception-backtrace-in-bytecode groundwork. No
`Bytes` module exists in this tree at all -- strings are still directly
mutable, consistent with a pre-safe-string-split, 1.07-era fork.

Sources
-------

`pad.org` (Backport list, NOT-BACKPORTED-ON-PURPOSE, Important commits in
OCaml history), `todo.org` (Last1/Last0, Components, Backports sections),
`changes.txt` (release entries, especially `* 0.3`),
direct inspection of `stdlib/*.mli` (`printexc`, `list`, `string`, `array`,
`hashtbl`, `stack`, `arg`, `sys`, `pervasives`, `lazy`), `typing/parmatch.ml`,
`typecore.ml`, `parsing/parser.mly`/`lexer.mll`, and `asmrun/backtrace.c`
in this tree, plus a targeted read of upstream's `Changes` file (versions
2.00 through 4.14.0, stopping before the multicore/effects era) in a
scratch clone of `github.com/ocaml/ocaml`, to catch anything the author's
own notes might have missed. Commit hashes are upstream SHA1s, cross-checked
against that clone where noted.
