---
type: enhancement
title: Faithful void → unit return semantics
impact: low
effort: low
status: done
---

`ret void` is currently translated by *fabricating* a return value: the
function `void @do_nothing()` becomes `do_nothing :: () -> Int` whose body
returns the integer `0` (`Translate.hs` `anfReturn … Nothing = Call (Const 0)`,
and `hsTypeOfLlvm` falls the `void` return type back to `Int`). This is
unsound: it conflates *"returns no value"* with *"returns the integer 0"*, and
the emitted type signature claims an `Int` result that the source function does
not produce. The comment in `TranslateAux.hsTypeOfLlvm` literally calls it a
fabrication.

The faithful functional encoding of `void` is the unit type `()`: a `void`
return type maps to `()`, and `ret void` maps to the unit value `()`. So
`do_nothing` becomes `do_nothing :: () -> ()` returning `()`.

There is a deeper observation worth recording in the TCC: the accepted subset
is **pure and side-effect-free** (no `load`/`store`/I/O), so a `void` function
carries no observable effect and no return value — it is *degenerate*. The only
such function expressible in the subset is one that does nothing. `ret void`
therefore exists only because LLVM emits it (e.g. for a C `void` function); it
has no computational content here. Mapping it to `()` is the minimal honest
translation and removes the `Int`-shaped lie without enlarging the subset.

## Implemented

`void` return type → `()`; `ret void` → the `Unit` value (the `Anf.Value`
constructor already existed but went unused for returns). The differential
harness gained a `void`/no-argument path: nullary functions are exercised once
(the empty input tuple is deterministic), and a `void` case certifies that the
unit translation compiles under GHC and runs — there is no computed value to
compare, so both sides emit a marker and the test confirms compile-and-run
parity. See [`plans/08-void-unit-semantics.md`](plans/08-void-unit-semantics.md).
