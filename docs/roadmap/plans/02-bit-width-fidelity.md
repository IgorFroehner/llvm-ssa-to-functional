---
type: plan
title: Implementation plan — bit-width faithful integer semantics
tracks: bit-width-fidelity.md
status: done
---

# Bit-width fidelity — implementation plan

Tracks [`../bit-width-fidelity.md`](../bit-width-fidelity.md). Goal: every LLVM
`iN` maps to the corresponding sized Haskell integer instead of collapsing into
64-bit `Int`, so wraparound, `trunc`, `zext` and `sext` are *semantically*
faithful — not just structurally correct.

## The core problem

The `Anf` AST throws away all type information: `Translate.hs` reads `Ast.Type`
on every node but never propagates it, and `PrintAnf.hs` emits untyped bindings.
GHC therefore defaults every value to 64-bit `Int`. To get faithful wraparound,
each binding must be pinned to an `IntN` type, and `trunc`/`zext`/`sext` must
emit real conversions.

## Width → Haskell type mapping

Round each `iN` *up* to the next available `Data.Int` width:

| LLVM        | Haskell |
|-------------|---------|
| `i1`, `i8`  | `Int8`  |
| `i9`–`i16`  | `Int16` |
| `i17`–`i32` | `Int32` |
| `i33`–`i64` | `Int64` |
| `> i64`     | `error` (out of subset) |

This handles the non-power-of-two `i33` in `examples/sum.ll` (→ `Int64`): the
`zext i32→i33; add; trunc i33→i32` sequence computes without overflow in
`Int64`, then wraps at the `trunc`.

## How correctness is anchored

Three annotation points, so every value's type is pinned and GHC's native
wraparound at `+`/`-`/`*` in `IntN` becomes automatic (no reliance on fragile
cross-lambda inference for loop-carried φ variables):

1. **Top-level type signature per function** — e.g. `factorial :: Int32 ->
   Int32`. Arg types from `ArgumentDef`, return from the `ret` `Type`. Anchors
   args + return.
2. **Per-binding annotation** — `aN = (expr) :: IntK`, where `K` comes from each
   decl's `Ast.Type` (already present on `BinOpCall`/`Icmp`/`ConvOpCall`/
   `Select`/`Call`). Anchors all internals.
3. **Explicit conversions at conv ops:**
   - `trunc M→N`: `fromIntegral x :: IntN`
   - `sext M→N`: `fromIntegral x :: IntN` (signed widen)
   - `zext M→N`: `fromIntegral (fromIntegral x :: WordM) :: IntN`

Inner block-lambda params do not need annotation — their types propagate from
call-site arguments, which are pinned by the binding annotations. (Verify by
building; fallback is pattern type sigs under `ScopedTypeVariables`.)

## Code changes

- **`src/Anf.hs`** — add a width/type tag. Attach to `Decl` (result type),
  `Function` (arg types + return type). `ConvOp` needs both source and target
  width.
- **`src/Translate.hs`** — stop discarding `Ast.Type`. Add `typeToWidth ::
  Ast.Type a -> Width`; thread the result type into each `Anf.Decl`, the
  function signature, and conv ops (`ConvOpCall` already carries both source and
  target `Type`).
- **`src/TranslateAux.hs`** — add `widthToHsType :: Width -> String`
  (`"Int8"`…`"Int64"`), `widthToWordType`, and `parseIntWidth :: String -> Int`
  (`"i33"` → `33`).
- **`src/PrintAnf.hs`** — emit the new header imports (`Data.Int`, `Data.Word`),
  the top-level signatures, `:: IntK` on each binding, and the conv-op
  conversions.
- **`src/Lexer.x`** — confirm non-standard widths like `i33` lex as a type token
  (they already do: `sum.ll` parses today).

## Signedness decision

Represent everything as signed `IntN`. The current corpus is all signed
arithmetic; the lone `lshr` operates on a provably non-negative value, so
arithmetic vs. logical shift coincide. Unsigned-op fidelity (`udiv`/`urem`/
`ult`/`lshr` on genuinely unsigned values) is documented as a known limitation
rather than built out now. (Confirmed acceptable with the author.)

## Differential harness (roadmap TODOs 2 & 3)

- **`test/differential/run.py`** — the appended driver does `print ((f x) ::
  Int)` and reads args `:: [Int]`. With typed functions this won't typecheck:
  read args as `Integer` and emit `print (fromIntegral (f (fromIntegral a1) …)
  :: Integer)` so the signed `IntN` result prints exactly as native's
  `(long long)` cast.
- Remove the `TRUNC` classification (`trunc()` + the `trunc_only` path) so width
  regressions become `MISMATCH` and fail the gate.
- Widen the `bin_search` sampler back to its full range (e.g. `(-100, 1<<20)`);
  the `m*m` i32-overflow control-flow divergence should vanish.

## Tests (TDD — already written)

`test/BitWidthSpec.hs` (red until implemented). Drives the real pipeline
(`runAlex … parseLLVMIR` → `translate` → `printProgram`) and asserts:

- **Type mapping** — `add1 :: Int32 -> Int32`; `:: Int32` on bindings; `import
  Data.Int` header; `no_overflow_square :: Int32 -> Int64`.
- **Conversions** — `sext` widens via `fromIntegral`; `i33` rounds up to
  `Int64`; `zext` goes through `Word32`; `trunc` narrows back to `:: Int32`.

Run: `stack test --test-arguments '--match "/bit-width fidelity/"'`.

## Verification

1. `stack build` — warning-clean (`-Wall`).
2. `stack test` — `ExamplesSpec` still parses all; `BitWidthSpec` goes green;
   update any golden `.hs` in `docs/{gcd,prime,safe_div}` for the new signatures
   and annotations.
3. `python3 test/differential/run.py` — every row **certified (exact)**,
   including the widened `bin_search`.
