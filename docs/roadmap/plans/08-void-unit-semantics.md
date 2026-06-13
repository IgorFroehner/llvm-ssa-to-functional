---
type: plan
title: Implementation plan — void → unit return semantics
tracks: 08-void-unit-semantics.md
status: done
---

# Void → unit semantics — implementation plan

Tracks [`../08-void-unit-semantics.md`](../08-void-unit-semantics.md). Goal:
stop fabricating an `Int` `0` for `void` functions; map the `void` return type
to `()` and `ret void` to the unit value, so `void @do_nothing()` translates to
`do_nothing :: () -> ()` returning `()`.

## The core problem

Two places fake an integer where the source has *no value*:

- `Translate.hs` — `anfReturn (Return _ _ Nothing) = Anf.Call (Anf.Const 0) []`
  emits `ret void` as the constant `0`.
- `TranslateAux.hsTypeOfLlvm` — falls `void` back to `Int`, so the signature
  reads `() -> Int`.

`PrintAnf` already prints the `Anf.Unit` value as `()` (`printValue Unit`), but
`printCall (Call Unit _)` was left `undefined` because no return ever produced
it.

## Code changes

- **`src/Translate.hs`** — `anfReturn (Return _ _ Nothing) = Anf.Call Anf.Unit
  []`.
- **`src/TranslateAux.hs`** — `hsTypeOfLlvm "void" = "()"` (before the integer
  fallback); refresh the now-stale "fabricates a `ret 0`" comment.
- **`src/PrintAnf.hs`** — `printCall (Call Unit _) = "()"` instead of
  `undefined`, so a `void` tail call prints the unit value.

No `Anf` AST change is needed — the `Unit` constructor already exists.

## Resulting output

```haskell
do_nothing :: () -> ()
do_nothing () =
  let
    a0 () =
      let
      in ()
  in a0 ()
```

## Differential harness (the no-input / void path)

`test/differential/run.py` assumed every case has ≥1 integer argument and an
integer result. Add support for:

- **No-argument functions** — pass `()` to the Haskell function instead of an
  empty argument list, and call the C function with no args. The empty input
  tuple is deterministic, so the existing `seen` de-duplication already runs it
  exactly once regardless of `-n`.
- **`void` return** — there is no value to compare. Both sides print a fixed
  marker (`()`): native calls the function then prints it; the pipeline does
  `print (f ())` (unit's `Show` yields `()`). Comparison is on stdout strings,
  so a `void` row is EXACT iff *both* executables built and ran. This certifies
  the unit translation compiles under GHC and runs — not a computed result,
  which `void` cannot have in this pure subset.
- Add the case: `ret_void.ll` / `do_nothing`, no args, `void` return.

## Verification

1. `stack build` — warning-clean (`-Wall`).
2. `stack test` — `ExamplesSpec` still parses `ret_void.ll`.
3. `stack run -- examples/ret_void.ll` — emits `do_nothing :: () -> ()`
   returning `()`.
4. `python3 test/differential/run.py ret_void` — the `void` row is
   `certified (exact)` (compiles + runs on both sides).
