# Differential testing of the translation

Validates the SSA→ANF translation *behaviourally*, beyond "the examples parse
and look right". For each example it builds two executables of the **same
function** and compares their output over a fuzzed range of integer inputs:

| build      | how                                                    | integer semantics |
|------------|--------------------------------------------------------|-------------------|
| `native`   | `clang` compiles the `.ll` directly + a tiny C driver  | true `iN` (32-/64-bit wraparound) |
| `pipeline` | this project translates `.ll`→Haskell, GHC compiles it | Haskell `Int` (64-bit) |

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
- **TRUNC** — `native == (pipeline truncated to N signed bits)`. The
  translation is structurally correct; the values differ only because Haskell
  `Int` is 64-bit. This is the bit-width unsoundness tracked in
  [`docs/roadmap/bit-width-fidelity.md`](../../docs/roadmap/bit-width-fidelity.md).
- **MISMATCH** — neither. A real translation bug; fails the run.

The samplers encode the subset's domain restrictions — e.g. `exp_mod` only
gets `mod ≠ 0` (division by zero is UB natively and an exception in Haskell),
and the pure-arithmetic kernels are fuzzed *across* the i32 overflow boundary
on purpose so the bit-width gap is exercised rather than hidden.

## Findings

With seed 1, 300 trials:

```
case                   tested  exact  trunc  mism  verdict
factorial:factorial        38     16     22     0  ok modulo i32 width
from_rust:factorial        38     16     22     0  ok modulo i32 width
fib:fib                    61     46     15     0  ok modulo i32 width
sum:asum                  300    163    137     0  ok modulo i32 width
square:square             300     62    238     0  ok modulo i32 width
square:no_overflow_square 300    300      0     0  certified (exact)
gcd:euclides_gcd          300    300      0     0  certified (exact)
bin_search:bin_search     299    299      0     0  certified (exact)
tot:phi                   299    299      0     0  certified (exact)
prime:is_prime            299    299      0     0  certified (exact)
safediv:safe_div          300    300      0     0  certified (exact)
select:safe_div           300    300      0     0  certified (exact)
mod_pow:exp_mod           300    300      0     0  certified (exact)
```

Two conclusions, exactly the dual outcome the roadmap item anticipated:

1. **The translation is structurally correct.** Every example matches native
   either exactly or after truncating to the IR's integer width — no genuine
   mismatch on any of the corpus. This covers loops/φ-nodes (`fib`, `sum`,
   `bin_search`, `tot`, `mod_pow`), recursion-as-tail-calls (`factorial`,
   `gcd`), `select`/ternary (`select`, `safediv`), and `i1`/boolean returns
   (`prime`). Notably it also confirms the signed-division choice in
   `TranslateAux` (`sdiv→quot`, `srem→rem`) matches C on negative operands.

2. **The bit-width unsoundness is real and measurable.** The `ok modulo i32
   width` rows are cases where 32-bit wraparound and 64-bit `Int` disagree;
   the translation only stays equivalent if the pipeline's output is truncated
   back to `i32`. This is precisely the motivation for
   [`bit-width-fidelity`](../../docs/roadmap/bit-width-fidelity.md): once `iN`
   maps to `Int8`/`Int32`/`Int64`, these rows should collapse to `EXACT`.

### A sharper edge: overflow that changes control flow

The benign `TRUNC` cases differ only in the *final value*. But bit-width
unsoundness can also flip a branch and make the outputs unrelated. `bin_search`
computes an integer square root via `if (m*m < n) …`. For `n` above ~90 000 the
intermediate `m*m` overflows `i32` natively while the 64-bit pipeline computes
it exactly, so the two take different loop paths and return different answers —
not recoverable by any output truncation:

```
$ python3 test/differential/run.py bin_search   # then widen its sampler
bin_search:bin_search   args=(694906,) native=146546 pipeline=834
```

(834 is the correct isqrt; 146546 is what the overflowing i32 IR actually
computes.) To keep MISMATCH meaning *"the translation broke"*, the harness
caps `bin_search` inputs at the no-overflow domain. The example is called out
here because it strengthens the bit-width case: faithful `iN` semantics aren't
just about the final value, they can change which path the program takes.
