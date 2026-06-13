---
type: research
title: Side effects via translation to monadic metalanguage
impact: high
effort: godmode
status: proposed
---

The Discussion section of the paper sketches the theory: ANF is equivalent to
Moggi's computational λ-calculus, so effects are representable. Cheapest
credible version: translate `load`/`store` (already lexed, not translated)
into a `State Memory` monad, or model division traps as `Maybe`/`Either`.

Key design question is granularity: does a whole function become monadic the
moment one instruction is effectful (simple, but destroys the clean tail-call
structure), or is purity inferred per block, lifting only where needed —
which is [effect-inference](effect-inference.md) wearing a different hat.
The two items are really one research direction.
