---
type: research
title: Effect/type inference over LLVM-IR (Rigon extension)
impact: high
effort: godmode
status: proposed
---

The stated long-term objective of the TCC: extend Rigon (2020)'s effect/type
inference, which worked over a purpose-built pseudo-imperative core, to real
LLVM-IR generated from C/C++/Rust.

Pragmatic first milestone: since the current subset is pure by construction,
every function should infer as effect-free (sanity baseline); then add one
effectful construct (e.g. `call` to an unknown declared function, or division
traps) and check the inference distinguishes it.

Likely prerequisite: [annotated-anf-ast](annotated-anf-ast.md). Closely tied
to [monadic-effects-translation](monadic-effects-translation.md) — inference
tells you *where* the monadic translation must apply.
