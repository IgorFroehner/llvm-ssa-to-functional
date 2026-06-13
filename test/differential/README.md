# Differential testing of the translation

Validates the SSA→ANF translation *behaviourally*, beyond "the examples parse
and look right". For each example it builds two executables of the **same
function** and compares their output over a fuzzed range of integer inputs:

| build      | how                                                    | integer semantics |
|------------|--------------------------------------------------------|-------------------|
| `native`   | `clang` compiles the `.ll` directly + a tiny C driver  | true `iN` (32-/64-bit wraparound) |
| `pipeline` | this project translates `.ll`→Haskell, GHC compiles it | sized Haskell `IntN` (`Int8`/`Int32`/`Int64`) |

Using `clang` on the **exact `.ll`** we translate (rather than the original
`.c`/`.rs`) makes `native` the ground truth for the IR's real semantics, with
no compiler-version drift between the two sides.

## Running

```bash
python3 test/differential/run.py            # all cases, 200 trials each
python3 test/differential/run.py -n 1000     # more trials
python3 test/differential/run.py factorial fib   # only some functions
python3 test/differential/run.py --seed 7    # different fuzz stream
```

Requires `clang` and `stack` on `PATH`; run from the repo root. Exit status is
non-zero **iff a genuine mismatch is found**, so it doubles as a CI gate.

## How a trial is classified

For a function returning `iN`:

- **EXACT** — `native == pipeline`.
- **MISMATCH** — otherwise. A real translation bug; fails the run.

Earlier runs had a third class, **TRUNC** — `native == (pipeline truncated to N
signed bits)` — for cases that were structurally correct but differed because
Haskell `Int` was 64-bit. That gap is closed: the translation now maps each LLVM
`iN` to the matching sized Haskell integer
([`bit-width-fidelity`](../../docs/roadmap/bit-width-fidelity.md)), so those
cases are EXACT and any surviving divergence is a genuine MISMATCH.

The corpus is **auto-discovered**: `run.py` scans `examples/*.ll` and reads each
function's signature straight off its `define` line, so dropping a new `.ll` in
`examples/` certifies it automatically — no per-example bookkeeping. (Files that
define `main`, and functions whose signature uses a type outside the subset, are
skipped.) Arguments are fuzzed with a default wide signed range that crosses the
i32 overflow boundary on purpose, so the bit-width gap is exercised rather than
hidden.

The one hand-written knob is `OVERRIDES` in `run.py`: a sampler is supplied for
a function *only* when the default range would leave the subset's domain — e.g.
`exp_mod` needs `mod ≠ 0` (division by zero is UB natively and an exception in
Haskell) and `ashr` needs a shift amount in `[0, 32)` — or when a counting loop
must stay bounded (`fib` would loop ~2³² times on a negative input).

**No-input and `void` cases.** A function with no arguments has a single,
deterministic input (the empty tuple), so it is run exactly once regardless of
`-n`. A `void`-returning function (`ret_void:do_nothing`,
[void→unit](../../docs/roadmap/08-void-unit-semantics.md)) has *no value* to
compare — in this pure subset a void function is degenerate. Both sides instead
print a fixed `()` marker, so its row is EXACT iff both executables built and
ran: it certifies the unit translation compiles under GHC and runs, not a
computed result.

## Findings

With seed 1, 300 trials, the whole corpus is bit-for-bit **EXACT**:

```
case                   tested  exact  mism  verdict
ashr:arith_shr            300    300     0  certified (exact)
bin_search:bin_search     300    300     0  certified (exact)
bool_ret:is_positive      300    300     0  certified (exact)
factorial:factorial       299    299     0  certified (exact)
fib:fib                    88     88     0  certified (exact)
freeze:freeze_inc         299    299     0  certified (exact)
from_rust:factorial       300    300     0  certified (exact)
gcd:euclides_gcd          300    300     0  certified (exact)
iabs:iabs                 300    300     0  certified (exact)
minmax:imin               300    300     0  certified (exact)
minmax:imax               300    300     0  certified (exact)
mod_pow:exp_mod           300    300     0  certified (exact)
prime:is_prime            300    300     0  certified (exact)
ret_void:do_nothing         1      1     0  certified (exact)
safediv:safe_div          300    300     0  certified (exact)
select:safe_div           300    300     0  certified (exact)
square:square             300    300     0  certified (exact)
square:no_overflow_square 300    300     0  certified (exact)
sum:asum                  300    300     0  certified (exact)
tot:phi                   300    300     0  certified (exact)
```

Two conclusions:

1. **The translation is structurally correct.** Every example matches native
   exactly — no divergence across loops/φ-nodes (`fib`, `sum`, `bin_search`,
   `tot`, `mod_pow`), recursion-as-tail-calls (`factorial`, `gcd`),
   `select`/ternary (`select`, `safediv`), `i1`/boolean returns (`prime`,
   `bool_ret`), and the broader-subset ops (`ashr`, `minmax`, `iabs`,
   `freeze`). It also confirms the signed-division choice in `TranslateAux`
   (`sdiv→quot`, `srem→rem`) matches C on negative operands.

2. **Bit-width fidelity holds.** The pure-arithmetic kernels are deliberately
   fuzzed *across* the i32 overflow boundary (`x*x`, triangular sums, `n!`), and
   native i32 wraparound and the pipeline's `IntN` wraparound agree exactly.
   The unsoundness that earlier surfaced as TRUNC rows is gone — these cases now
   certify EXACT, which is precisely the outcome
   [`bit-width-fidelity`](../../docs/roadmap/bit-width-fidelity.md) targeted.

### A sharper edge: overflow that changes control flow

Bit-width unsoundness isn't only about a final value — it can flip a branch.
`bin_search` computes an integer square root via `if (m*m < n) …`. For `n` above
~90 000 the intermediate `m*m` overflows `i32`. *Before* bit-width fidelity the
64-bit pipeline computed `m*m` exactly while native wrapped, so the two took
different loop paths and returned unrelated answers:

```
bin_search:bin_search   args=(694906,) native=146546 pipeline=834
```

(834 is the true isqrt; 146546 is what the overflowing i32 IR actually
computes.) Now that the pipeline computes `m*m` in `i32` too, both sides take
the same path: the case is EXACT across its full input range, which is why its
sampler no longer has to dodge the overflow domain. The example is kept here
because it shows *why* faithful `iN` semantics matter — they can change which
path the program takes, not just the number it prints.
