---
type: testing
title: Differential testing of the translation
impact: high
effort: low
status: done
---

Validate the translation itself, beyond "the examples compile and look right".
Compile each `.c` source both natively (clang → binary) and through this
pipeline (clang → `.ll` → Haskell → GHC), fuzz inputs over the integer domain,
and compare outputs. Builds directly on the existing `examples/` corpus.

Cheap to build, strengthens every other roadmap item, and would either certify
the method or surface the bit-width unsoundness described in
[bit-width-fidelity](bit-width-fidelity.md).

## Implemented

`test/differential/run.py` (see `test/differential/README.md`). Refinement on
the original sketch: the native side compiles the **`.ll` itself** with clang
(not the `.c`), so it is ground truth for the exact IR we translate, with no
compiler-version drift. Each trial is classified EXACT / TRUNC (matches after
truncating to the IR's integer width) / MISMATCH; the run fails only on
MISMATCH, so it serves as a CI gate.

Outcome: **both** predicted results materialised. The whole `examples/` corpus
is structurally correct (no genuine mismatch — covering loops/φ-nodes,
tail-call recursion, `select`, and `i1` returns), while five pure-arithmetic
kernels match only after i32 truncation, quantifying the
[bit-width-fidelity](bit-width-fidelity.md) unsoundness. `bin_search` further
shows that overflow can change control flow, not just the final value.
