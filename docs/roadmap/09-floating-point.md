---
type: enhancement
title: Floating-point support (pure, two-type story)
impact: medium
effort: medium
status: proposed
---

Split out of [broader-subset](03-broader-subset.md): floating point is still
*pure* (no side effects), but it is the first feature that breaks the
"everything is `Int`" assumption baked into the translator and `PrintAnf`.

`float`/`double` map to Haskell `Float`/`Double`, and the `f*` instruction
family (`fadd`/`fsub`/`fmul`/`fdiv`/`frem`, `fcmp`, `fpext`/`fptrunc`,
`sitofp`/`uitofp`/`fptosi`/`fptoui`) needs its own rows in the `TranslateAux`
tables. The hard part is not the table rows — it is that operand and result
types can no longer be assumed integer, so the translator needs a **small type
discipline** to decide, per value, whether it is an `IntN` or a floating type
and to emit the right conversions at the boundaries.

This is why it is its own item rather than a cheap win inside
[broader-subset](03-broader-subset.md): it forces a two-type representation and
a (small) type checker, which is also a natural stepping stone toward the
type-annotated AST of [annotated-anf-ast](04-annotated-anf-ast.md).

Read-only aggregates / `getelementptr` over constant arrays are theoretically
pure too, but clang rarely emits something clean enough to be worth it — out of
scope here.
