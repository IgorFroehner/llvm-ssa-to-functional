---
type: enhancement
title: Bit-width faithful integer semantics
impact: high
effort: medium
status: done
plan: plans/02-bit-width-fidelity.md
---

Today `i8`/`i32`/`i64` all collapse into Haskell `Int`, so wraparound semantics
are silently wrong: `add` overflow, `trunc`, and `zext`/`sext` don't actually
do anything. Map `iN` to `Int8`/`Int32`/`Int64` (`Data.Int`/`Data.Word`) so
the generated code is *semantically* faithful, not just structurally.

This upgrades the paper's claim from "produces runnable Haskell" toward
"produces equivalent Haskell", and is directly testable via
[differential-testing](differential-testing.md).

## Evidence (from differential testing)

`test/differential/run.py` already quantifies this gap. The translation is
structurally correct on the whole `examples/` corpus, but five pure-arithmetic
kernels match the native `iN` semantics **only after their output is truncated
to i32** — i.e. they are wrong by exactly the wraparound this item fixes:

| example | function | divergence |
|---------|----------|------------|
| `factorial` / `from_rust` | `factorial` | `n!` wraps i32 for `n ≥ 13` |
| `fib` | `fib` | wraps i32 around `n = 47` |
| `sum` | `asum` | triangular number wraps i32 |
| `square` | `square` | `x*x` wraps i32 |

`bin_search` shows the *sharper* form: an i32-overflowing `m*m` flips a loop
branch, so the result is unrelated to the 64-bit pipeline's (not merely a
truncation of it). The harness currently caps `bin_search` inputs to dodge this.

## TODOs for this item

- [x] Map `iN` → `Int8`/`Int16`/`Int32`/`Int64` (`Data.Int`) in the emitted
      Haskell; make `trunc`/`zext`/`sext` actually convert.
- [x] Once landed, the differential harness must go all-green at **EXACT**:
      remove the `TRUNC` allowance in `run.py` (see its
      `TODO(bit-width-fidelity)`) so width regressions fail the gate.
- [x] Widen the `bin_search` sampler back to its full range — the
      control-flow divergence should vanish.

Implementation plan and outcome: [`plans/bit-width-fidelity.md`](plans/bit-width-fidelity.md).
All 13 differential cases are now bit-for-bit EXACT. Signedness of genuinely
*unsigned* ops (`udiv`/`urem`/`ult`/`lshr` on unsigned values) is the one
documented limitation — everything is represented as signed `IntN`.
