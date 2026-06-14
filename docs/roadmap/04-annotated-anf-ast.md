---
type: refactor
title: Annotated ANF AST with multiple backends
impact: medium
effort: medium
status: done
---

Today `Translate` targets `Anf.hs` and `PrintAnf` emits Haskell text; there
are no annotation slots. Refactor so `Anf` can carry metadata (effects, types)
and `PrintAnf` becomes one of several backends.

Enabler for [effect-inference](05-effect-inference.md) and
[monadic-effects-translation](06-monadic-effects-translation.md); not valuable on
its own.

## Findings (post-implementation)

Full write-up in [`plans/04-annotated-anf-ast.md`](plans/04-annotated-anf-ast.md);
the points that change how the *next* items proceed:

- **The "types" half of the premise was already done.** Item
  [09](09-floating-point.md) had already replaced `Anf`'s string type slots with
  a first-class `Ty`. So 04's real scope was only the *derived*-annotation slot
  (effects) plus the backend seam — `Ty` stays constructor data (it directs
  codegen; it is not erasable metadata) and was deliberately **not** folded into
  the annotation.
- **Uniform parameterisation was the load-bearing decision.** Adding the
  annotation to *every* node (mirroring `Ast a`) and deriving `Functor` +
  `Foldable` means erasure, relabelling and effect-aggregation are all free —
  #05's bottom-up join is a `foldMap`, not a bespoke traversal. The cost (unused
  labels on leaf `Value`/`BinOp` nodes) is harmless.
- **The `docs/{gcd,prime,safe_div}` goldens are NOT a byte-exact oracle.** Each
  differs from live output by one trailing blank line, and this is pre-existing
  on `main` (live output is identical to `main`). The plan names golden-diff as
  the primary regression gate — for #05/#06 the trustworthy oracle is "diff
  against `main`'s output" + the differential harness, not the `docs/` goldens.
  Worth regenerating those goldens at some point.
- **Forward-compat contract verified.** #05 is now a body-only change to
  `Effect.annotate` (+ richer `Effect`); #06 is one new `Backend` value reading
  the `Effect` label. Neither needs to touch `Anf`'s structure or `Translate`.
