---
type: plan
title: Implementation plan — widen the accepted LLVM-IR subset (cheap wins)
tracks: 03-broader-subset.md
status: done
---

## Outcome

All three cheap wins landed: `ashr` (Lexer + `TranslateAux` row), `freeze`
(full token→AST→print path, lowered to a typed identity alias), and the
`llvm.abs`/`smin`/`smax` rewrite in `Translate` (`intrinsicRewrite`). Examples
`ashr`/`minmax`/`iabs`/`freeze`/`bool_ret` are in `examples/` (parse gate) and
certified bit-for-bit **exact** by the differential harness; unit assertions
live in `test/TranslateSpec.hs` under *broader subset*.

The differential harness caught a **latent shift bug**: Haskell's `shiftR` takes
the amount as `Int`, but LLVM types both shift operands at the same `iN`, so
`x \`shiftR\` n` failed to typecheck once `n` was a (sized) *variable* rather than
a literal — a gap that `shl`/`lshr` had too but no prior example exercised.
`PrintAnf.printBinOp` now coerces the shift amount (`fromIntegral`).

# Broader subset (cheap wins) — implementation plan

Tracks [`../03-broader-subset.md`](../03-broader-subset.md). Goal: accept more of
the LLVM-IR that clang actually emits for **pure, integer-only** functions,
without touching the core SSA→ANF algorithm. Floating point is explicitly *not*
here — it needs a two-type story and lives in
[`../09-floating-point.md`](../09-floating-point.md).

Each feature below is independent and shippable on its own. (1)–(3) are the
actual scope — pure table/grammar rows, no second value type. (4) is recorded
only to say explicitly why idiomatic `Bool` is *not* here (it moved to
[`../10-boolean-types.md`](../10-boolean-types.md)).

## Build the example corpus first

Before writing any mapping, capture a real clang-emitted `.ll` per feature so the
work is test-driven and the differential harness can certify it. Generate from C
with `clang -O1 -S -emit-llvm -fno-discard-value-names`, mirroring the existing
`examples/sources/*` convention (source kept beside the `.ll`).

While a feature is still unsupported its `.ll` **must not** sit in `examples/`
top-level — `test/ExamplesSpec.hs` globs `examples/*.ll` and asserts they all
parse, so it would go red. Stage candidates in `examples/broader-subset/`
(a subdir; `listDirectory` there is non-recursive and only picks `.ll` by
extension at the top level, so the subdir is ignored). **As each feature lands,
promote its example to `examples/` top-level** so `ExamplesSpec` parses it and
`test/differential/run.py` certifies it bit-for-bit.

Target sources:

- `ashr.c` — `int f(int x, int n){ return x >> n; }`
- `minmax.c` — `int mn(int a,int b){return a<b?a:b;} int mx(int a,int b){return a>b?a:b;}`
- `iabs.c` — `int g(int x){ return x<0?-x:x; }`
- `bool_ret.c` — `_Bool p(int x){ return x>0; }`

## (1) `ashr` — missing binop variant

`src/Lexer.x` lists `lshr` (line 63) but not `ashr`. In Haskell, `shiftR` on a
signed `IntN` *is* arithmetic, so it maps exactly like `lshr` already does.

- **`src/Lexer.x`** — add `<0> ashr { createToken BinOp }` next to `lshr`.
- **`src/TranslateAux.hs`** — add `"ashr" -> " \`shiftR\` "` to `translateOperator`.
  (Note the existing signedness caveat from [bit-width-fidelity](02-bit-width-fidelity.md):
  everything is signed `IntN`, so `lshr` and `ashr` collapse to the same `shiftR`.
  That is correct for `ashr`; `lshr` on a genuinely-unsigned value stays the
  documented limitation.)

No parser change — `binOpCall` already covers it.

## (2) `freeze`

`%y = freeze iN %x`. For the pure subset, `freeze` is a no-op: it only blocks
undef/poison propagation, so semantically `y = x`.

- **`src/Lexer.x`** — new keyword token `Freeze`.
- **`src/Parser.y`** — token decl + a `freezeCall` production
  (`freeze typeAnotation value`) and an `assignment` alternative
  `lname '=' freezeCall`.
- **`src/Ast.hs`** — a `Freeze` node (type + operand) and a `DecFreeze` decl
  constructor.
- **`src/Translate.hs`** — translate to an ANF binding `y = x` (reuse the value
  path; no new ANF decl kind strictly needed — can lower to an alias).
- **`src/PrintAnf.hs`** — emit `y = x :: IntN` (keep the bit-width annotation so
  it stays faithful, per [bit-width-fidelity](02-bit-width-fidelity.md)).

## (3) Integer intrinsics — `llvm.abs`, `llvm.smin`, `llvm.smax`

These are already *parsed*: they are `call` instructions and go through the
`funcCall` production into `DecCall`. They are also already *printable* — there
is no new ANF construct. `PrintAnf.printCall` (`src/PrintAnf.hs:44`) renders a
`Call (Name f) vs` as `f v1 v2 …`, so an intrinsic is just a `Call` with a
different callee name. The whole fix is a **name + argument rewrite** on the
existing node:

| LLVM intrinsic   | Rewrite to            | Prints as | Notes |
|------------------|-----------------------|-----------|-------|
| `@llvm.abs.iN`   | `Call (Name "abs") [x]` | `abs x`   | drop the trailing `i1` immarg (is_int_min_poison) |
| `@llvm.smin.iN`  | `Call (Name "min") [a,b]` | `min a b` | two real args |
| `@llvm.smax.iN`  | `Call (Name "max") [a,b]` | `max a b` | two real args |

`abs`/`min`/`max` are Prelude functions, so no header import is needed, and the
result keeps the existing `:: IntN` binding annotation.

Two facts the code pins down (don't get these wrong):

- **Match on the *normalized* callee.** `NameNormalizer` strips punctuation during
  parsing, so by the time `Translate` sees the name it is already `llvmabsi32`,
  not `@llvm.abs.i32`. The matcher keys on the normalized prefixes `llvmabs` /
  `llvmsmin` / `llvmsmax`. (Keeping the mapping in `Translate` is correct — it is
  a semantic rewrite — even though matching the dotted form would read nicer.)
- **Only `llvm.*` is rewritten.** `call` is *not* intrinsic-only in this subset —
  same-module function calls are why `DecCall` exists. Anything not matching an
  `llvm.` family must fall through `anfCall` (`src/Translate.hs:154`) unchanged.

- **`src/Translate.hs`** — in `anfDec`/`anfCall`, route `DecCall` through a small
  `intrinsicCall :: String -> [Anf.Value] -> Maybe Anf.Call` helper that matches
  the normalized prefix and rebuilds the `Call` (dropping `llvm.abs`'s second
  arg); `Nothing` falls through to the existing path. One row per future
  intrinsic (`umin`/`umax`/`ctpop`/…).
- **`src/PrintAnf.hs`** — no change; the rewritten `Call` prints by the existing
  rule.

Resolved: rewrite-in-Translate over generated `llvm.*` prelude shims — shims would
emit ugly `llvmabsi32 x 0`, need dead-code elimination to avoid unused helpers,
and still require the same detection.

## (4) Idiomatic `Bool` — *out of scope*, moved to its own item

Real `i1`-aware translation (keep `icmp` results as `Bool`, feed `Anf.IfThenElse`
directly, return `Bool` for `i1`) is **not** a cheap win: the `zext i1 … to i32`
that clang emits constantly forces a `Bool → Int` coercion — the same two-type
discipline that put floating point in [`../09-floating-point.md`](../09-floating-point.md).
It now lives in [`../10-boolean-types.md`](../10-boolean-types.md).

Nothing to do here: the faithful `0/1` encoding already works (`i1` rounds up to
`Int8` via [bit-width-fidelity](../02-bit-width-fidelity.md), so a `_Bool`-returning
function already returns `Int8` `0/1` = C's `bool`). The `bool_ret.c` example
stays in the corpus as a *regression* guard for that, not as new work.

## Tests

- **`test/ExamplesSpec.hs`** — promoting each example to `examples/` extends the
  parse gate automatically (no code change).
- **New `test/BroaderSubsetSpec.hs`** (TDD, red first) — drive the real pipeline
  (`runAlex … parseLLVMIR` → `translate` → `printProgram`) and assert the emitted
  Haskell for each feature: `\`shiftR\`` for `ashr`, the `y = x` alias for
  `freeze`, `abs`/`min`/`max` for the intrinsics.
  Run: `stack test --test-arguments '--match "/broader subset/"'`.
- **`test/differential/run.py`** — once an example is in `examples/` top-level it
  should certify **exact** against native, same gate as the rest of the corpus.

## Verification

1. `stack build` — warning-clean (`-Wall` plus the `-W…` set in `package.yaml`).
2. `stack test` — `ExamplesSpec` parses all (now including the promoted
   examples); `BroaderSubsetSpec` goes green.
3. `python3 test/differential/run.py` — new rows certified exact.
4. Edit `package.yaml` (hpack) if a new spec module is added, never the generated
   `.cabal`.
