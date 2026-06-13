---
type: enhancement
title: Widen the accepted LLVM-IR subset (still pure)
impact: medium
effort: medium
status: proposed
---

Increase the surface of LLVM-IR accepted while keeping the pure-functions
restriction:

- Cheap wins: missing `icmp`/binop variants, `freeze`, intrinsics clang emits
  at `-O1` (`abs`, `smin`/`smax`-style); `i1`-aware translation that keeps
  booleans as `Bool` instead of re-encoding as `if … then 1 else 0`. Mostly
  `Lexer.x`/`Parser.y` plus rows in the `TranslateAux` tables.
- Floating point: still pure, maps to `Double`, but breaks the "everything is
  `Int`" assumption in the output — first step that needs a two-type story
  (a small type checker) rather than a table row.
- Read-only aggregates/`getelementptr` over constant arrays: theoretically
  pure, but clang rarely emits something clean enough; probably not worth it.
