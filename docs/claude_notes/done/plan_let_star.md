Plan: `let*`/`let+` binding-operator sugar
============================================

Investigated 2026-09-15, following up on `todo.org`'s "add/backport
cool/recent features (e.g., let*...)" wishlist item.

This is a REPORT / plan only -- nothing has been implemented yet.

Why this is not a cherry-pick
---------------------------------

The real feature (`let*`, `let+`, `and*` etc.) is OCaml PR #1947,
"Add monadic let operators" (Leo White, merged 2018-11-27, released
in 4.08). Its real diff is ~6500 lines across 34 files, but ~9800 of
those lines are a regenerated menhir bootstrap parser (irrelevant --
we use ocamlyacc). The hand-written core is still substantial:
`parser.mly` (33 lines), `typecore.ml` (302 lines -- the actual
desugaring/type-checking), `translcore.ml` (67 lines), `lexer.mll`
(7 lines), plus mechanical updates to ~15 AST-traversal files
(pretty-printer, mapper, iterator, typed-tree printer, etc.) that all
need to know about the new `Pexp_letop` parsetree constructor.

Checked our own tree before assuming a patch could apply: our
`parsetree.mli` is 224 lines with no attributes/extension-node
mechanism at all, our `typecore.ml` is 839 lines *total* (upstream's
diff alone to that file is 302 lines, against a modern file that's
thousands of lines long), and our `lexer.mll` has no `kwdopchar`-style
infrastructure. 18 years and many feature-generations of divergence --
`git apply`/`--3way` would fail almost everywhere, the same way 3-way
merges already failed on much smaller, more local diffs during the
Int32/Int64 work. **This will be a from-scratch reimplementation of
the feature's observable behavior, hand-adapted to ocaml-light's much
simpler grammar and AST, using the real feature's design as a
reference -- not a diff to apply.** Concretely: no original-author
commits here, no `[partial]`/cherry-pick trailers -- every commit for
this feature is `Co-Authored-By: Claude Sonnet 5`, since none of it is
literally upstream's code.

The simplification that makes this tractable
--------------------------------------------------

Real OCaml's implementation adds a new `Pexp_letop` parsetree node and
teaches the type-checker to desugar it during type-checking. We don't
need any of that: `let* p = e1 in e2` can be desugared **at parse
time**, directly into the exact same AST a hand-written
`(let*) e1 (fun p -> e2)` would produce -- an ordinary `Pexp_apply` of
an ordinary `Pexp_ident`, applied to a `Pexp_function`. Since that's
plain, everyday syntax our compiler already fully understands, **zero
changes are needed to `parsetree.mli`, `typecore.ml`, `translcore.ml`,
or any of the ~15 AST-traversal files upstream had to touch.** The
entire feature is a lexer addition plus two small grammar rules.

This mirrors an existing pattern in `parser.mly` almost exactly:
`mkinfix arg1 name arg2 = mkexp(Pexp_apply(mkoperator name 2, [arg1;
arg2]))` (used for `expr INFIXOP0 expr` etc.) already builds
`Pexp_apply(Pexp_ident(Lident name), [...])` from an operator-name
string and its operands -- `let* p = e1 in e2` needs exactly that,
just with the second operand being a freshly-built `Pexp_function`
instead of a plain expression.

Concrete grammar plan
--------------------------

**1. Lexer (`parsing/lexer.mll`)**: add one rule, using the
`symbolchar` character class that already exists (line 179-180, used
for `INFIXOP*`/`PREFIXOP`):

```
| "let" symbolchar +   { LETOP (Lexing.lexeme lexbuf) }
```

Placed anywhere in the `token` rule (ocamllex is longest-match-wins,
so this automatically outranks the plain `lowercase identchar*` rule
for e.g. `let*`, without needing to reorder anything -- `letrec`,
`let` alone, etc. are unaffected since they don't match `"let"
symbolchar+` at all: either nothing follows `let` at all, or the
following character isn't a `symbolchar`). Token payload is the *full*
matched text (`"let*"`, `"let+"`, ...), because that's the exact
identifier name a user's `let (let*) x f = ...` definition binds.

No `ANDOP`/`"and" symbolchar+` rule for this MVP -- see scope below.

**2. Parser tokens (`parsing/parser.mly`)**: add
`%token <string> LETOP` next to the other `<string>`-carrying operator
tokens (`PREFIXOP`, `INFIXOP0`..`INFIXOP4`, `SUBTRACTIVE`).

**3. Grammar, definition site** (`let (let*) x f = ...`): add `LETOP`
as one more alternative in the existing `operator:` nonterminal
(line 868-884), right where `PREFIXOP`/`INFIXOP0`/etc. already live.
Since `val_ident: LIDENT | LPAREN operator RPAREN` (line 864-866) and
`let_binding: val_ident fun_binding` (line 632-633) already route
through `operator` for *any* parenthesized-operator definition (this
is how `let (+) a b = a + b` already works today, unchanged), this one
addition is the *entire* definition-site change. Nothing else to do.

**4. Grammar, use site** (`let* p = e1 in e2`): add one new `expr`
alternative next to the existing `LET rec_flag let_bindings IN
seq_expr` rule (line 440-441):

```
| LETOP pattern EQUAL seq_expr IN seq_expr %prec prec_let
    { mkinfix $4 $1 (mkexp(Pexp_function([$2, $6]))) }
```

(`%prec prec_let` matches the existing plain-`let` rule and the
`let_binding`'s own `pattern EQUAL seq_expr %prec prec_let`, line 635 --
same precedence bucket, no new conflicts expected.) This alone
produces `Pexp_apply(Pexp_ident(Lident "let*"), [e1; fun p -> e2])`,
i.e. exactly `(let*) e1 (fun p -> e2)`.

`pattern` (not just a bare identifier) is used deliberately: real
OCaml allows `let* Some x = opt in ...`-shaped refutable patterns
there too. Our `pattern` nonterminal already supports constructors,
tuples, aliases, or-patterns, etc. (line 674-690), so this parses fine
structurally.

`let+`, `let>>=`, or any other `let<op>` spelling all fall out for
free from the same two rules, since `LETOP`'s payload is whatever
symbolchars followed `let` -- no extra grammar needed per spelling.

Deliberately out of scope for this MVP
-------------------------------------------

- **`and*` (multi-binding/applicative combination)**. Real OCaml's
  `let* p1 = e1 and* p2 = e2 in e3` desugars using *both* `(let*)` and
  `(and*)` together (roughly `(let*) ((and*) e1 e2) (fun (p1,p2) ->
  e3)`). Skipping entirely for the MVP -- no `ANDOP` token, no grammar
  rule. Single-binding `let*`/`let+` is the overwhelmingly common case
  (Option/Result-style chaining) and needs none of this.
- **Refutable-pattern failure handling**. Real OCaml's desugaring for
  a refutable pattern like `let* Some x = opt in ...` is more careful
  than ours will be: it needs the binding operator's *matching*
  behavior (an optional extra function, conventionally
  `(let*).unhandled` or similar) to convert a failed match into
  whatever the monad's "failure" means, rather than raising
  `Match_failure`. Our plain `Pexp_function([pat, body])` desugaring
  will just raise `Match_failure` if the pattern doesn't match at
  runtime -- a real, documented behavioral gap vs. real OCaml, not
  silently swallowed. Fine for irrefutable patterns (the common case:
  `let* x = e in ...` with a plain variable) and constructor patterns
  that are known-total for the type in question.
- **Attributes on binding operators** (a later, separate upstream RFC
  refinement, #11398) -- not applicable, we don't have an attributes
  mechanism at all.

Nice-to-have, cheap add-on: stdlib operators
--------------------------------------------------

claude: correction, checked after the fact -- this was wrong. Real
OCaml's `Option`/`Result` did *not* get binding operators as part of the
4.08 rollout; verified via a local clone of upstream (`git log -S'let (
let* )' -- stdlib/option.ml stdlib/result.ml`) that they got them only
much later, `@since 5.5` (`90d454692` "Add Option.product and
Option.Syntax", `60b30b936` "Add Result.product and Result.Syntax"), and
in a different shape: a nested `Syntax` submodule with `let*`/`and*`/
`let+`/`and+` built around a new `product` function, not a flat
`Option.(let*)`. Since we don't target OCaml 5 and this postdates 4.14
anyway, there's no real upstream commit to attribute the addition below
to in the range we care about -- it's our own addition, same as the
original (mistaken) reasoning concluded, just for the right reason now.

claude: final decision (discussed with pad) -- don't add them. Since
there's no real upstream commit to port from in the range we target,
adding `Option.(let*)`/`Result.(let*)` here would just be us inventing
stdlib API surface, which cuts against the "light"/minimal-footprint
spirit of this fork. Instead: `stdlib/option.ml`/`.mli` and
`stdlib/result.ml`/`.mli` each get a short comment next to `bind`/`map`
noting the one-liner (`let ( let* ) o f = bind o f`) a user would write
themselves to get `let*` for that type -- exactly the pattern pad
already uses in `semgrep-pfff-libs/commons/core/Common.ml`
(`let ( let* ) = Option.bind`). The only real stdlib change is
re-exposing `Result.bind` (see below) -- `Option.bind`/`map` were
already public.

Testing plan
----------------

No existing upstream testsuite to port (same reason as the rest --
too divergent). Plan: a small `test/Moretest`-style `.ml` exercising
`Option`/`Result` chains via a locally-defined `let ( let* ) = bind`
(since neither ships its own), `let+`, a refutable-pattern case
documented as expected-`Match_failure`-if-mismatched, and a
user-defined custom operator (`let (let*) x f = ...` at top level) to
confirm the definition-site grammar change works standalone.

Implementation order
-------------------------

1. Lexer + token + `operator:` grammar addition (definition site) --
   small, mechanical, testable immediately with `let (let*) x f = f x`
   at the toplevel.
2. `expr` grammar addition (use site) -- the `mkinfix`/`Pexp_function`
   rule above.
3. `stdlib/result.ml`/`.mli`: re-expose `bind` and fix its
   type-inference bug (see below); no Option/Result changes needed
   beyond that.
4. Test file + `make world`/`test`/`check` verification, plus a live
   REPL smoke test (same discipline as the Int32/Int64 work).
5. `changes.txt` entry once landed.

Single commit is probably fine given the small size (lexer + 2 grammar
rules + stdlib one-liners) -- unlike the Int32/Int64 chain there's no
separate "faithful cherry-pick" vs "adjustment" split to make, since
none of this is upstream's literal code. All `Co-Authored-By: Claude
Sonnet 5`.
