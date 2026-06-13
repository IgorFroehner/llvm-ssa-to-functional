---
type: enhancement
title: i1-aware Bool (idiomatic boolean output)
impact: low
effort: medium
status: proposed
---

Today every `icmp` is re-encoded as `if … then 1 else 0` and every conditional
`br` tests `v /= 0`, so booleans round-trip through `Int`. An `i1`-aware
translation keeps an `icmp` result as a Haskell `Bool`, feeds it straight into
`Anf.IfThenElse`, and lets an `i1` return type be `Bool`.

Split out of [broader-subset](03-broader-subset.md): this looks like a cheap win
but isn't. The moment an `icmp` result is a `Bool`, you hit the `zext i1 … to
i32` that clang emits constantly, which needs a `Bool → Int` coercion at the
boundary — i.e. the **same two-type / type-discipline problem** that put floating
point in its own item ([floating-point](09-floating-point.md)). #03's dividing
line is "needs no second value type / type checker"; real `Bool` fails it.

Note the *minimal* behaviour already works and is faithful: `i1` rounds up to
`Int8` under [bit-width-fidelity](02-bit-width-fidelity.md), so a `_Bool`-returning
function already returns `Int8` `0/1` — exactly C's `bool`. This item is purely
about emitting *idiomatic* `Bool` instead, which is why its impact is low.

Shares the type-discipline machinery with [floating-point](09-floating-point.md)
and is a natural client of the type-annotated AST in
[annotated-anf-ast](04-annotated-anf-ast.md); sequence it alongside those rather
than with the #03 table rows.
