# Roadmap

Future-work directions distilled from the paper's Future Work section
(`docs/texts/full-paper-en.md`) and follow-up discussion. Each item carries
`type`, `title`, `impact`, `effort`, `status` in its frontmatter. Effort scale:
low / medium / high / godmode (the hardest research jumps).

| #  | Item | Type | Impact | Effort | Status |
|----|------|------|--------|--------|--------|
| 01 | [Differential testing of the translation](01-differential-testing.md) | testing | high | low | done |
| 02 | [Bit-width faithful integer semantics](02-bit-width-fidelity.md) | enhancement | high | medium | done |
| 08 | [Faithful void → unit return semantics](08-void-unit-semantics.md) | enhancement | low | low | done |
| 05 | [Effect/type inference over LLVM-IR](05-effect-inference.md) | research | high | godmode | proposed |
| 06 | [Side effects via monadic metalanguage](06-monadic-effects-translation.md) | research | high | godmode | proposed |
| 03 | [Widen the accepted LLVM-IR subset](03-broader-subset.md) | enhancement | medium | medium | done |
| 09 | [Floating-point support (pure, two-type story)](09-floating-point.md) | enhancement | medium | medium | proposed |
| 10 | [i1-aware Bool (idiomatic boolean output)](10-boolean-types.md) | enhancement | low | medium | proposed |
| 04 | [Annotated ANF AST with multiple backends](04-annotated-anf-ast.md) | refactor | medium | medium | proposed |
| 07 | [Source-to-source optimizations on ANF](07-anf-optimizations.md) | research | low | high | proposed |
