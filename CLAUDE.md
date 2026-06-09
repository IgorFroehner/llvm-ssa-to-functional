# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A compiler, written in Haskell, that translates **LLVM-IR in SSA form** into an
**executable functional program (ANF / A-Normal Form) in Haskell**. It is the
implementation behind an undergraduate CS final paper (TCC, in Portuguese under
`docs/texts/TCC_V2/main.md`). The theoretical basis is the proven equivalence
between SSA and functional programming, and the SSA→ANF translation procedure of
Chakravarty, Keller & Zadarnowski (2003) / Kelsey (1998).

The translation is intentionally restricted to a **subset of LLVM-IR**:
- Only simple integer types — no arrays, pointers, or composite/aggregate types.
- No side-effecting instructions (no I/O, no syscalls, `load`/`store`/`getelementptr`
  are lexed/declared but not translated).
- **All registers and blocks must be named, including the first block.** The
  parser does not synthesize a label for an implicit entry block (the commented-out
  `initialStatementsBlock` in `Parser.y` was the abandoned attempt at this).

## Commands

Build / run use Stack (GHC 9.8.2, see `.github/workflows/haskell.yml`):

```bash
stack build                                  # build (runs alex + happy codegen)
stack test                                   # run the whole hspec suite
stack run -- <file.ll> [-o <out.hs>]         # translate LLVM-IR -> Haskell ANF
stack run -- --graph-viz <file.ll>           # emit DOT of the control-flow graph
stack run -- --dominance-viz <file.ll>       # emit DOT of the dominator tree
```

Run a single test / subset (hspec `--match` on `describe`/`it` text):

```bash
stack test --test-arguments '--match "/translate/"'
stack test --test-arguments '--match "parses all examples"'
```

`examples/*.ll` are the corpus that `test/ExamplesSpec.hs` asserts must all parse.
`docs/{gcd,prime,safe_div}/` hold worked end-to-end examples (`.c` source, `.ll`
input, `.hs` expected output, plus CFG/dominance PNGs).

## Architecture: the translation pipeline

The flow is `app/Main.hs` → Lexer → Parser → (Dominance) → Translate → PrintAnf.
There are **two distinct ASTs**: `Ast` (LLVM-IR side) and `Anf` (output side).
`Translate` is the only bridge between them.

1. **`src/Lexer.x`** (Alex, `monadUserState-bytestring` wrapper) — tokenizes
   LLVM-IR. Produces `RangedToken`s carrying a source `Range`. Many LLVM keywords/
   attributes (`nsw`, `align`, metadata `!…`, `attributes`, etc.) are deliberately
   *skipped* rather than tokenized. Every AST node is parameterized over `a` and
   carries a `Range` (`Ast.Program Range`, etc.) for potential error reporting.

2. **`src/Parser.y`** (Happy, monadic, threaded through the Alex monad) — builds
   `Ast.Program Range`. During parsing, **names are normalized** via
   `src/NameNormalizer.hs`: local `%x` registers and block labels become valid
   Haskell identifiers by stripping punctuation and prefixing `a` (so `%3` → `a3`,
   block `6:` → `a6`); globals (function names) just have punctuation stripped.
   This is why generated Haskell never collides with `%`/`@` sigils.

3. **`src/Dominance.hs`** — uses `fgl` (`Data.Graph.Inductive`). `buildGraph`
   turns a function's basic blocks into a CFG (`Gr String ()`, node 0 = entry =
   first block in source order). `dominance` computes the immediate-dominator
   relation (`iDom`) and returns it as an **inverted graph** (edge dominator→dominated),
   i.e. the dominator tree. This tree drives ANF nesting.

4. **`src/Translate.hs`** — the core SSA→ANF algorithm (`F` in `NOTES.md` and the
   paper). Per function:
   - Build CFG, then dominator tree.
   - `anfFromTree` walks the **dominator tree** recursively from the entry. **Each
     basic block becomes a lambda**; the lambdas for the blocks it *immediately
     dominates* are nested inside its scope (`suc` on the dominator tree). This
     nesting is the whole point — it reconstructs lexical scope from dominance.
   - **φ-nodes become the lambda's parameters** (`argsFromPhis`); a block with no
     φ's takes `()`.
   - **Branches become tail calls** (`tailCallFromBlock`). For each successor, the
     call's arguments are resolved by looking up, in the *target* block's φ-nodes,
     the value tagged with the *current* (source) block label
     (`getValueForCurrentLabel`). Conditional `br` → `Anf.IfThenElse`; `ret` →
     a tail call returning the value.
   - LLVM ops are mapped 1:1 to ANF decls (`DecBinOp`/`DecIcmp`/`DecSelect`/
     `DecCall`/`DecConvOp`).

5. **`src/PrintAnf.hs`** — pretty-prints `Anf.Program` to Haskell source text
   (manual indentation via `indent`/`indentEach`, `printf` templates). Emits an
   `import Data.Bits` header (for `.&.`, `shiftL`, etc.). Operator/comparison
   spelling lives in `src/TranslateAux.hs` (`translateOperator`, `translateCmpType`):
   `add`→`+`, `sdiv`→`` `div` ``, `slt`→`<`, etc. `icmp` is rendered as
   `if a < b then 1 else 0` so it stays an integer value; conditional branches test
   `v /= 0`.

6. **`src/GraphViz.hs`** — trivial DOT emitter for the `--graph-viz` /
   `--dominance-viz` modes; shares the same `Gr String ()` from `Dominance`.

### Output ANF shape

A whole LLVM function → one top-level Haskell function whose body is a `let` of the
entry block's lambda plus a call to it. Within, nested lambdas mirror the dominator
tree. See the `README.md` `factorial` example for the canonical input→output pair.
Grammar of the generated ANF variant (multi-arg lambdas, primitive ops, conditionals)
is in `docs/texts/TCC_V2/main.md` §3.2; the translation function `F` is §3.3 and
`NOTES.md`.

## Conventions & gotchas

- `Ast.hs` has several commented-out constructors (`FunctionDec`, `SCall`) — these
  are deliberately unimplemented branches of the subset, not dead code to delete.
  Many functions are intentionally partial (`undefined`/`error`) on the unsupported
  subset; don't "fix" them into total functions without checking the subset scope.
- Two `Program`/`Function`/`Call`/etc. types exist (`Ast.*` vs `Anf.*`); always
  qualify and keep clear which side of the pipe you're on.
- `-Wall` plus many `-W…` flags are on (see `package.yaml`); keep builds warning-clean.
- Edit `package.yaml` (hpack), **not** the generated `llvm-ir-to-functional.cabal`.
- Lexer/parser are generated from `.x`/`.y` by `alex`/`happy` at build time; edit
  the `.x`/`.y` sources, never any generated `.hs`.
