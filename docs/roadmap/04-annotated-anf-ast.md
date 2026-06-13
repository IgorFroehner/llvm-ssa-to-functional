---
type: refactor
title: Annotated ANF AST with multiple backends
impact: medium
effort: medium
status: proposed
---

Today `Translate` targets `Anf.hs` and `PrintAnf` emits Haskell text; there
are no annotation slots. Refactor so `Anf` can carry metadata (effects, types)
and `PrintAnf` becomes one of several backends.

Enabler for [effect-inference](effect-inference.md) and
[monadic-effects-translation](monadic-effects-translation.md); not valuable on
its own.
