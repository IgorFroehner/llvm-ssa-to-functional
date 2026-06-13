---
type: enhancement
title: Widen the accepted LLVM-IR subset (still pure)
impact: medium
effort: medium
status: done
plan: plans/03-broader-subset.md
---

Increase the surface of LLVM-IR accepted while keeping the pure-functions,
integer-only restriction. These are the **cheap wins**: missing `icmp`/binop
variants (`ashr`), `freeze`, and integer intrinsics clang emits at `-O1`
(`llvm.abs`, `llvm.smin`/`llvm.smax`). Each is mostly `Lexer.x`/`Parser.y` plus
rows in the `TranslateAux` tables — no change to the core SSA→ANF algorithm.

The dividing line for this item: a feature qualifies as a cheap win **iff it
needs no second value type / type checker**. Three things fail that test and are
deliberately *out*, because they break the "everything is `Int`" assumption:

- Floating point → its own item, [floating-point](09-floating-point.md).
- Idiomatic `Bool` output (real `i1`-aware translation) → its own item,
  [boolean-types](10-boolean-types.md). The faithful `0/1` encoding already
  works, so this is only about idiomatic output.
- Read-only aggregates / `getelementptr` over constant arrays — theoretically
  pure but clang rarely emits something clean enough; not worth it for now.

Implementation plan: [`plans/03-broader-subset.md`](plans/03-broader-subset.md).
