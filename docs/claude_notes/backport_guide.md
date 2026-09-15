Guide: backporting upstream OCaml commits into ocaml-light
============================================================

Practices learned while cherry-picking the Custom-blocks/Int32/Int64 chain
(see `plan_custom_int64.md`). Applies to any future "port commit X from
upstream ocaml/ocaml into this fork" effort.

Workflow: two commits per upstream commit, sometimes three
------------------------------------------------------------

For each upstream commit being ported:

1. **Cherry-pick commit** -- apply the upstream change as literally as
   possible. Keep the original author, date, and commit message. Add a
   trailer noting the exact upstream SHA, e.g.
   `(cherry picked and forward-ported from upstream commit <sha>)`.
   No `Co-Authored-By` line on this commit. If `docs/literate/*.nw` books
   weave any of the touched files, regenerate them (`make sync_c` or
   equivalent in `docs/literate/`) and fold that into *this* commit too --
   it's just the doc-side mirror of the same content, not a new decision.
2. **Adjustment commit** -- fix whatever is needed to make it build/work in
   ocaml-light's context (missing files, diverged constants, syncweb
   markers, genuine upstream bugs that block the OCaml compiler, etc). This
   one gets the usual `Co-Authored-By: Claude Sonnet 5` trailer.

Sometimes a commit needed to reach a *buildable, testable* state actually
belongs to a **later** upstream commit -- e.g. the commit being ported
introduces a bug (or leaves a call site stale) that upstream itself only
fixed one or more commits later. When that later fix is small and
self-contained, pull it forward as its own **partial cherry-pick commit**
between the two above: same treatment as step 1 (original author, that
*later* commit's date and message, a trailer citing that later SHA, no
`Co-Authored-By`), just scoped to only the lines actually needed. This
happened twice in the Custom-blocks/Int32/Int64 chain: item #0's
`Reverse_*` call-site rename lived inside item #1's commit, and item #1's
GC-crashing bug was only fixed by item #2's commit. Ask before doing this
-- it's a judgment call about how far to reach forward, not a mechanical
default.

Show the diff/commit metadata and wait for go-ahead before actually
running `git commit` -- one commit at a time, reviewed. Run the full
build/test verification *before* committing, not after -- but note that
"before committing" means before the *cherry-pick commit lands in a form
that's supposed to build*, i.e. after all of the commits above (including
any pulled-forward partial cherry-pick) are staged together. A single
upstream commit was sometimes never independently buildable upstream
either (see the bugs section below); insisting on green-build-per-commit-
in-isolation fights the history rather than reflecting it faithfully.

Getting the upstream commit
----------------------------

Full-tree `git cherry-pick <sha>` does **not** work -- ocaml-light and
upstream ocaml/ocaml are unrelated histories (no common ancestor known to
git), so every file conflicts as add/add. Instead:

```sh
git fetch --depth=2 https://github.com/ocaml/ocaml.git <sha>:refs/some-tag
git tag some-tag-parent $(git rev-parse refs/some-tag~1)
git tag some-tag-tip refs/some-tag
```

then work file-by-file with `git diff some-tag-parent some-tag-tip -- <path>`.

A local blob-less clone of the upstream repo (if one exists in the
session's scratchpad from earlier archaeology) **cannot** be used as a
`git fetch`/`cherry-pick` source for this -- it's a promisor remote itself
and refuses to lazily re-serve missing blobs to another local repo
("lazy fetching disabled"). Fetching directly from
`https://github.com/ocaml/ocaml.git` works fine and is simpler.

Applying per file: patch first, hand-edit only the conflicts
---------------------------------------------------------------

**Never hand-retype file content from a diff/`git show`.** Transcription
errors happen (a dropped trailing space caused a byte-level mismatch on a
supposedly "identical" new file in this session). Always apply mechanically:

```sh
git diff <parent> <tip> -- <path> > /tmp/x.patch
git apply --check /tmp/x.patch        # try a clean apply first
git apply /tmp/x.patch                # if clean, just do it
# if it fails:
git apply --3way /tmp/x.patch         # falls back to a 3-way merge,
                                       # leaving <<<<<<< conflict markers
                                       # ONLY around the hunks that don't
                                       # apply -- everything else lands
                                       # automatically, correctly
```

For a wholesale new file (not a diff, the whole blob is new), extract it
directly instead of retyping:

```sh
git show <tip>:<path> > <path>
```

A `--3way` conflict almost always narrows down to a *small* region (one or
two hunks out of a whole file), because the diff's context lines fail to
match ours exactly. Resolve just that region by hand; trust the rest of
the auto-merged file.

Why patches conflict, and what each cause means
---------------------------------------------------

- **syncweb chunk markers break context matching.** Any file with
  `/*s: ... */`/`/*e: ... */` markers interleaved in the code will make a
  plain `git apply` fail even for a logically-trivial change, because the
  upstream diff's context lines don't have those markers. This is the
  single most common conflict cause in this codebase. See the dedicated
  section below.
- **Genuine, unrelated pre-existing divergence.** Our fork sometimes
  already has a different value/name/design at the exact spot the patch
  touches (a tuning constant, an enum spelling, a storage strategy) that
  has nothing to do with the feature being ported. When resolving,
  **keep our fork's local choice** and apply only the actual feature
  change on top of it -- do not silently drag in unrelated upstream
  renames/tuning (e.g. keep `White` instead of switching to `Caml_white`,
  keep a locally-tuned resource-limit constant instead of overwriting it,
  keep an embedded-storage design instead of switching to upstream's
  pointer-indirection design if that's what our fork already had).
- **The hunk doesn't apply to us at all.** Sometimes the referenced
  function/file doesn't exist in ocaml-light because it was added to
  upstream sometime between the 1.07 fork point and the commit being
  ported, for a reason unrelated to the feature at hand (debug-only heap
  checker, a `finalise.c` split that never happened here, a completely
  different implementation of a whole subsystem like `otherlibs/str`'s
  regexp engine). Recognize this by checking whether the surrounding
  function/symbol exists in our tree at all before fighting the patch --
  if it doesn't, skip that hunk/file entirely rather than force it in.
- **Our fork's file has evolved far beyond the 1.07 baseline already.**
  If a `git apply --check` fails everywhere in a file (not just one
  region), check the file's line count / structure against upstream's
  pre-commit version (`git show <parent>:<path> | wc -l` vs current). A
  large gap means the file was substantially rewritten locally after the
  fork and the whole hunk may be inapplicable -- verify by grepping for
  the symbols the diff touches (e.g. `alloc_final`) before attempting to
  force a merge.
- **A clean `git apply` isn't automatically semantically correct either.**
  Context-based apply can still produce a duplicate when our fork already
  had equivalent-but-differently-phrased content that the upstream diff's
  context didn't recognize as the same thing (e.g. a `.mli` value
  declaration that already existed under slightly different formatting
  got a second, redundant copy inserted next to it). After any "clean"
  apply, grep for the newly-touched symbol to make sure it wasn't already
  present nearby.

syncweb markers: the hard rule
----------------------------------

**Never add, modify, or move an existing `/*s: ... */` / `/*e: ... */`
marker.** These are syncweb chunk markers tied to the literate-programming
`.nw` books under `docs/literate/`; altering them breaks `make sync`.
This applies even when a marker's *name* no longer quite matches what's
inside it after a port (e.g. a marker named `[[Reverse_int32]]` ends up
wrapping the new `Reverse_32` macro) -- leave the marker text and position
exactly as they are; a mismatched label is acceptable, an altered/moved
marker is not.

**Do not add new markers either, even for code that's genuinely new and
even in a file that's actively woven into a `.nw` book with one marker per
item.** It's tempting (every neighboring function has its own marker) but
wrong -- marker creation is the author's call, tied to deciding how the
new material should be explained in the book. Leave brand-new code
unmarked.

**When a port would delete code that's wrapped by an existing marker**
(e.g. upstream renames/rewrites a macro whose old body is the entire
content of its own dedicated marker), don't delete it -- wrap the old body
in `#if 0` / `#endif` right where it was, keep the marker exactly as is,
and add the new code alongside (still inside the same marker, still
unmarked itself). This preserves both the marker and a readable trace of
what was replaced. This is different from an ordinary single-line edit
*inside* a much bigger marked function (e.g. changing one `case` label in
a switch) -- that's just a normal code edit, no `#if 0` needed, since nothing
marked is being emptied out.

Files with **no** syncweb markers at all (check with
`grep -c '/\*s:' <path>`) have none of these constraints -- edit/patch them
normally. Whether a file is "actively woven" can be checked against
`SRC_C`/`SRC_ML` etc. in `docs/literate/Makefile`.

Faithfully-reproduced historical bugs
-----------------------------------------

Upstream commits from this era sometimes contain genuine bugs that were
fixed moments/days later in a subsequent real commit (e.g. `major_gc.c`
assigning a `struct custom_operations *` where a function pointer was
needed, fixed the very next plan item one day later) or that are simply
latent/dead code paths never exercised by our build (e.g. a missing
function prototype causing an implicit-int-truncated-to-pointer bug in a
fallback path that nothing in our tree actually calls after the port).
**Reproduce these faithfully in the cherry-pick commit** -- don't silently
"fix" upstream's history. Verify with `git show <sha>:<path>` directly
against the raw commit blob (not a computed diff) that the bug is really
there and not an artifact of the patching process.

The exception is a bug that will **hard-fail the build** for something
our test suite actually exercises (a C warning is harmless; an OCaml type
error is not, and neither is a bug in a code path that's actually live,
e.g. all our `alloc_final` callers got migrated to `alloc_custom` in the
same commit, making the buggy legacy `alloc_final` path dead -- but a
straight OCaml typo like `oc` for `ic` in a function that ships in
`otherlibs/threads` is not dead code). For those, ask before fixing, and
distinguish two shapes of the problem:
- **A real typo/bug in code that otherwise applies to us** -- fix inline
  in the adjustment commit (keep the buggy version faithfully in the
  cherry-pick commit first, matching upstream).
- **A hunk that depends on something this fork genuinely doesn't have**
  (e.g. a whole new stdlib primitive like `Unix.open_process_full` that
  our `otherlibs/unix` never got) -- this isn't a bug to fix, it's another
  instance of "the hunk doesn't apply to us at all" above. Drop that
  piece from the cherry-pick commit entirely rather than including it
  broken-then-fixed; note it as skipped in the commit message.

When a crash reproduces a bug you already flagged as "faithfully
preserved," confirm it's really that bug (not something the patching
introduced) with a live backtrace before asking how to handle it:
`gdb --batch -ex run -ex bt --args <the failing command>`. Seeing the
exact function names from the suspected bug in the backtrace (e.g.
`sweep_slice` jumping straight into a `custom_operations` global instead
of calling through it) is strong, checkable evidence, not a guess.

Verification
---------------

Full sequence before committing: `make world`, `make opt`, `make test`,
`make check` (semgrep). A `gcc -fsyntax-only` pass on just the touched C
files is a fast way to catch conflict-resolution mistakes (stray
`<<<<<<<` markers, missing includes) before running the full build.
`make test` can fail on a stale `.cmo`/`.cmi` left over from before an
interface changed (error like "make inconsistent assumptions over
interface X") -- that's a leftover build artifact, not a real bug; `rm`
the stale files in that test directory and rerun.
