---
type: research
title: Source-to-source optimizations on the ANF output
impact: low
effort: high
status: proposed
---

The paper's "apply optimization algorithms to the output" future-work item,
reframed to be meaningful: GHC already optimizes the output regardless, so
"run it through optimizers and see" proves nothing. Instead, pick an
optimization that is awkward in SSA (classic example from Chakravarty et al.:
code motion across the dominance restriction), implement it as a
source-to-source pass on `Anf.Program`, and show it is simpler or equally
powerful compared to the SSA equivalent.

A nice self-contained follow-up paper, but lowest impact-per-effort on this
roadmap.
