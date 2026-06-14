---
type: plan
title: Implementation plan — annotated ANF AST and the algebra of backends
tracks: 04-annotated-anf-ast.md
status: done
---

## Outcome

Landed. `src/Anf.hs` is now parameterised over an annotation type `a` (uniformly,
mirroring `Ast a`) and derives `Functor`/`Foldable`, giving label rewriting,
erasure (`Anf.erase = fmap (const ())`) and aggregation (`foldMap`) for free.
`Translate` produces the bare `Anf.Program ()`; a new `src/Effect.hs` carries the
`Effect` join-semilattice (`Pure`/`⊥`, lawful `Semigroup`/`Monoid`) and the
`annotate :: Program () -> Program Effect` pass (currently the constant-`Pure`
baseline — exactly #05's sanity milestone). A new `src/Backend.hs` makes a backend
a record of functions (`backendName`, `render`); `PrintAnf` is the first
instance (`haskellBackend`), made annotation-blind (`printProgram :: Program a ->
String`), and a second, annotation-*reading* backend `src/AnnotDump.hs` proves
the seam. `app/Main.hs` runs `parse → translate → annotate → render` and gained a
`--backend NAME` flag.

`Ty` stayed constructor data (it directs codegen; not folded into the annotation
— plan §0/D2). Verification matched §6: the whole corpus is **byte-identical to
`main`** (confirmed by direct output diff on `gcd`/`prime`/`safe_div`, not just
the stale goldens) and still **bit-for-bit exact** through the differential
harness (24 functions); `test/AnnotatedSpec.hs` encodes T1 (annotation
transparency / naturality), T2 (pipeline factorisation), the seam proof (a second
backend emits different text and surfaces the `Pure` label the Haskell backend
never prints), the all-`Pure` baseline (`all (== Pure)` over the `Foldable`
instance), and the lattice laws. Build is warning-clean under the full `-W` set;
70 examples / 0 failures.

The forward-compat contract (§7) holds as designed: #05 is now a *body* change to
`Effect.annotate` (+ richer `Effect`) with no signature/pipeline churn, and #06 is
one new `Backend` value reading the `Effect` label — neither touches `Anf`'s
structure or `Translate`.
---

Tracks [`../04-annotated-anf-ast.md`](../04-annotated-anf-ast.md). Goal: give the
output AST a place to carry *derived* analysis results (effects first, anything
later) and turn `PrintAnf` from *the* output into *one* output among several,
without changing a single byte of the Haskell currently emitted.

This item is a **refactor**, not a feature: it adds no new accepted LLVM-IR and
no new generated code on the existing corpus. Its entire value is structural — it
is the seam that the two godmode research items,
[`../05-effect-inference.md`](../05-effect-inference.md) (compute *where* effects
live) and [`../06-monadic-effects-translation.md`](../06-monadic-effects-translation.md)
(emit monadic code *there*), both plug into. The roadmap is explicit that it is
"not valuable on its own"; the discipline of this plan is therefore to (a) state
the theory cleanly enough that the paper can reuse it, and (b) define a
verification that proves the refactor is *behaviour-preserving* and the new seam
is *real*, since there is no user-visible feature to demo.

---

## 0. What item 09 already did, and what is left

The roadmap line says "so `Anf` can carry metadata (effects, types)." Half of
that is already true. [`09-floating-point`](09-floating-point.md) replaced the
string type slots in `Anf` with a first-class `Ty` and centralised the type
discipline in `src/TypeSystem.hs`. So **types are not metadata in this codebase —
they are intrinsic structural fields that *direct codegen*** (`rho`, `coerce`,
NaN-faithful `fcmp`, …). They are not optional and must not be folded into a
generic annotation slot, or the printer loses the information it needs.

That distinction is the spine of this plan and is itself a theoretical point
(§2.1): there are two kinds of decoration on the tree —

- **Essential / structural** (`Ty`): part of the term's meaning, consumed by
  *every* backend, present from construction. Already done.
- **Derived / analytic** (effects, and future analyses): computed by a *pass*
  over an already-built term, consumed by *some* backends, absent until inferred.
  This is what 04 must add.

So 04 reduces to exactly two moves:

1. **A generic annotation parameter** on `Anf`, mirroring how `Ast a` carries a
   `Range`, so a pass can decorate the tree with derived facts (§2, §4).
2. **A backend abstraction** so the same annotated tree can be rendered more than
   one way, with the current printer as the first instance (§3, §4).

---

## 1. Where the current design stops being enough

- `src/Anf.hs` is **monomorphic**. `data Decl = DeclBinOp String Ty BinOp | …`
  has nowhere to write "this binding is pure / may trap / calls an unknown
  function." [`05-effect-inference`](../05-effect-inference.md) needs that slot at
  *binding*, *block*, and *function* granularity; there is currently no slot at
  any granularity.
- `src/PrintAnf.hs` **is** the output: `printProgram :: Program -> String` is
  called directly from `app/Main.hs`. There is no notion of "a backend." The
  monadic translation in [`06`](../06-monadic-effects-translation.md) is a
  *different rendering* of the same term (`do { x <- … }` where today we emit
  `let x = … in`), but the current shape forces it to be either a fork of
  `PrintAnf` or a pile of conditionals inside it.
- `Translate` builds the fully concrete tree and hands it straight to the
  printer. There is no point in the pipeline where an analysis pass could run
  between construction and rendering, because the tree has no slot for the pass
  to write into.

Note the asymmetry with the *input* side: `Ast a` is already parameterised over
an annotation (`Range`) and even derives `Foldable` over it (`src/Ast.hs:1`).
The output side never got the same treatment because, until effects, nothing
needed to decorate it. This plan brings `Anf` up to the same standard as `Ast`.

---

## 2. Theoretical foundation: decorated terms

### 2.1 Two decorations, one of them new

Formally, treat the ANF abstract syntax as a functor `A` whose fixed point
`μA` is the set of ANF terms. Item 09 baked the type assignment into `μA`
itself (every former carries its `Ty`), because types are *needed to define the
denotation* `⟦·⟧` of an ANF term — a `DeclBinOp` over `TyDouble` and one over
`TyInt 32` denote different functions. Types are therefore not an annotation;
they are constructor data.

An **annotation**, by contrast, is extra data hung on each node that does *not*
change `⟦·⟧`. The classic way to model it is the **cofree comonad** `Cofree A α`,
the type of `A`-trees every node of which additionally carries a label of type
`α`. Two standard facts make this the right frame:

- `Cofree A α` is a `Functor` in `α`: `fmap g` relabels every node, structure
  untouched. In particular `() <$ t` **erases** all labels.
- It is a `Comonad`: `extract` reads the root label; `extend` recomputes every
  node's label from its whole subtree — which is exactly the shape of a
  *bottom-up effect analysis* (a node's effect is the join of its children's).

We do **not** import `Control.Comonad.Cofree` or the `recursion-schemes`
package — that machinery is overkill for an undergraduate codebase and would
obscure the algorithm. Instead we realise the *same* idea the way `Ast` already
does it: **add a final type parameter `a` to every `Anf` datatype and derive
`Functor` and `Foldable`** (§4.1). `Anf.Program a` is then exactly `Cofree A a`
specialised and unrolled, with:

- `fmap` (from `DeriveFunctor`) = relabelling / erasure (`fmap (const ()) p`),
- `foldMap` (from `DeriveFoldable`) = aggregate every label, e.g. "the join of
  all effects in this function" for free.

The base instantiation `Anf.Program ()` is the unlabelled tree and must render
**byte-identical** to today's output — that is the regression theorem (§5, §6).

### 2.2 The annotation payload: an effect lattice

What 05 will write into the slot is an **effect**. State it now (even though 04
only ships the trivial element) so the slot has the right algebraic shape and 05
does not have to re-parameterise:

> Effects form a bounded **join-semilattice** `(E, ⊔, ⊥)`. `⊥ = Pure`; `e₁ ⊔ e₂`
> is "exhibits both effects." For the current pure subset the only inhabited
> point is `⊥`, so the baseline analysis labels every node `Pure` — which is
> precisely 05's stated sanity milestone ("every function infers effect-free").

The lattice (not just a set) matters because the analysis is a **fold up the
dominator-tree-shaped term**: a block's effect is the join of its bindings' and
its successors' effects; a function's is the join over its blocks'. `foldMap`
into the `Semigroup` `(E, ⊔)` (with `mempty = ⊥` as `Monoid`) gives that
aggregation directly from the `Foldable` instance — the reason §2.1 insists on
deriving `Foldable`. The concrete `data Effect` and the inference live in 05;
04 ships only the `Pure`/`⊥` carrier and the plumbing.

### 2.3 Backends as algebras over the term

A backend is a function `μA → R` (`R = String` today). The principled name for a
structurally-recursive such function is an **`A`-algebra** `A R → R` folded by a
catamorphism `cata :: (A r -> r) -> μA -> r`. `PrintAnf` is already this fold
written by hand (each `print*` handles one constructor and recurses). The
theoretical content of "multiple backends" is therefore:

> Fix the carrier `μA`. Each backend is one algebra. The Haskell printer, a
> future C printer, the monadic printer of [`06`](../06-monadic-effects-translation.md),
> and a debug pretty-printer are four algebras over the *same* term. Adding a
> backend never touches the term or the other backends — the open/closed
> property we are buying.

We will **not** rewrite `PrintAnf` as an explicit `cata` (again: avoid
`recursion-schemes`); the hand-written recursion *is* the algebra. What we add
is the *interface* that names "an algebra producing output" so backends are
interchangeable and selectable (§3, §4.3).

### 2.4 The two theorems this refactor must respect

The translation theorem from the [09 plan](09-floating-point.md) (§3.1, "`F` is
type-preserving") is about the *typed* term and is **unaffected**: `Ty` stays
constructor data, so `F` still lands in the same typed language. Two *new*,
small theorems pin the refactor down and double as its test oracle (§6):

> **(T1 — Annotation transparency / naturality.)** Let `B` be the Haskell
> backend and `p : Anf.Program a` any annotated program. Then
> `B(p) = B(fmap (const ()) p)`. I.e. the Haskell backend factors through
> erasure: annotations never reach the emitted text.
>
> *Why it holds:* `B`'s clauses pattern-match only on constructors and `Ty`
> fields, never on the annotation. Mechanically true once the slot is added and
> ignored. This is what guarantees the corpus output is unchanged.

> **(T2 — Pipeline factorisation.)** The old `translate` then `printProgram`
> equals the new `render Haskell ∘ annotate ∘ translate'` on every input, where
> `translate' = fmap (const ()) ∘ … ` produces the unlabelled term and
> `annotate` is the (currently trivial, all-`Pure`) effect pass.
>
> *Why it holds:* `annotate` only writes labels (changes `a`, not structure),
> and by T1 the Haskell backend ignores labels. So inserting `annotate` is
> observationally invisible on the Haskell backend — but it is the hook 05/06
> need.

T1 is "the refactor didn't change the output"; T2 is "the new pass slot is wired
in and inert." Together they are the precise sense in which a behaviour-preserving
refactor *can* be verified despite having no visible feature (§6).

---

## 3. Design decisions to settle before coding

These are the choices that shape the diff; the recommendation in each case is
the one the rest of the plan assumes.

**D1 — Annotation granularity: uniform vs. selective.** Either parameterise
*every* `Anf` datatype over `a` (uniform, mirrors `Ast`), or add a slot only to
`Function`/`Lambda`/`Decl` (the nodes 05 labels). *Recommend uniform.* It mirrors
`Ast a` exactly (one idiom in the codebase, not two), and — decisively — it lets
us `deriving (Functor, Foldable)` and get erasure (T1) and effect-aggregation
(§2.2) for free. Selective slots would need hand-written traversals. The cost is
annotations on a few nodes nothing reads (`Value`, `BinOp`); harmless.

**D2 — Keep `Ty` as constructor data (do *not* move it into the annotation).**
Per §0/§2.1, `Ty` directs codegen and is consumed by every backend; it is not
optional metadata. Folding it into `a` would force every backend to assume a
type-bearing annotation and would break the clean "annotation = derived,
erasable" story (T1 would be false). *Recommend: `Ty` stays where 09 put it.*

**D3 — Backend interface shape.** A lightweight `Backend` typeclass with one
method `render :: Program Effect -> String`, vs. a record of functions, vs. a
plain `data Target = …` dispatched in `Main`. *Recommend the typeclass* (one
instance per backend module; §4.3). Make every backend's signature take the
*annotated* `Program Effect` so the pipeline is single and uniform; the Haskell
backend is polymorphic/ignores the label (T1). This couples nothing — the
Haskell backend does not depend on 05 — while giving 06's monadic backend the
annotation it needs from the same entry point.

**D4 — Always run `annotate`, even while trivial.** The pipeline becomes
`translate (→ Program ())` → `annotate (→ Program Effect)` → `render`. Running
the (all-`Pure`) pass now, rather than when 05 lands, is what makes T2 testable
today and means 05 is a *body* change to `annotate`, not a *pipeline* change.

---

## 4. Implementation, file by file

### 4.1 `src/Anf.hs` — add the parameter, derive the instances

- Add a final type parameter `a` to every datatype: `Program a`, `Function a`,
  `Lambda a`, `Expr a`, `Decl a`, `ConvOp a`, `Select a`, `Icmp a`, `Flow a`,
  `BinOp a`, `Call a`, `IfThenElse a`, `Value a`, `ArgumentDef a`.
- The label sits on the nodes an analysis cares about; for uniformity (D1) put
  one `a` field on each *recursive* node. Concretely the load-bearing ones:
  `Function`, `Lambda` (a block — the natural unit of per-block purity), and each
  `Decl` (per-binding effect). Leaf-ish nodes (`Value`, `BinOp`) still take `a`
  to keep the `Functor`/`Foldable` derivation total, even though nothing reads
  them — same as `Range` sits on every `Ast` node today.
- `{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}`; `deriving (Show, Eq, Functor,
  Foldable)`. `Foldable` requires `a` to be the *last* parameter and every
  recursive position to thread it — the uniform scheme guarantees this.
- Export nothing new beyond the now-parameterised constructors.

This is the bulk of the mechanical churn; it touches signatures everywhere `Anf.`
types are named but is otherwise rote.

### 4.2 `src/Translate.hs` — produce `Program ()`

- `translate :: Ast.Program Range -> Anf.Program ()`. Every `Anf.` constructor
  application gains a `()` in the annotation slot. Nothing else changes — the
  dominator-tree walk, φ→λ-args, branch→tail-call, and `Ty` elaboration are all
  untouched. `Ty` continues to flow exactly as today (§0/D2).
- Optionally provide `Functor`-based helpers, but `()` literals at each site are
  simplest and keep the walk readable.

### 4.3 New: the backend abstraction

- New module `src/Backend.hs`:

  ```haskell
  class Backend b where
    backendName :: b -> String          -- for the CLI / errors
    render      :: b -> Anf.Program Effect -> String
  ```

  (`Effect` imported from the new `Effect` module, §4.5; while trivial it is a
  one-constructor `Pure`.)

- `src/PrintAnf.hs` becomes the **Haskell backend**: keep all current code, add

  ```haskell
  data Haskell = Haskell
  instance Backend Haskell where
    backendName _ = "haskell"
    render _ = printProgram        -- printProgram now :: Program a -> String
  ```

  The only change to the printer body is the type: `printProgram :: Program a ->
  String` (polymorphic in the annotation, ignores it — the mechanical witness of
  T1). Every `print*` helper similarly gains `… a -> …` and ignores the slot.

- Add a *second, real* backend to prove the seam (this is the deliverable that
  makes the refactor checkable — §6): `src/AnnotDump.hs`, a debug backend that
  pretty-prints the *annotated* tree (block/binding labels included), i.e. the
  first consumer that actually reads `a`. It need not be pretty Haskell — its job
  is to demonstrate (and test) that a backend *other than* the Haskell printer
  can read the annotation the Haskell backend ignores. It doubles as a debugging
  aid for 05.

### 4.4 `app/Main.hs` — select a backend, thread the pass

- New flag, e.g. `--backend haskell|annot-dump` (default `haskell`), parsed
  alongside the existing `--graph-viz` / `--dominance-viz` modes.
- Pipeline: `parse → translate → annotate → render chosenBackend`. Wire
  `annotate` (§4.5) unconditionally (D4).

### 4.5 New: `src/Effect.hs` — the (trivial) annotation pass

- `data Effect = Pure deriving (Eq, Show)` with `Semigroup`/`Monoid` instances
  realising `(⊔, ⊥)` (currently `Pure <> Pure = Pure`, `mempty = Pure`) so 05
  inherits the lattice shape (§2.2).
- `annotate :: Anf.Program () -> Anf.Program Effect`, currently `fmap (const
  Pure)`. That one line is the whole pass today; 05 replaces its *body* with the
  real bottom-up inference (using `foldMap` for the join), changing **no
  signatures and no pipeline wiring** — the forward-compat contract (§7).

### 4.6 `src/GraphViz.hs`, `src/Dominance.hs`

- Untouched. They operate on `Ast`/the CFG, not on `Anf`.

### 4.7 `package.yaml`

- Register `Backend`, `Effect`, `AnnotDump` (and the new spec module). Edit
  hpack, never the generated `.cabal` (per CLAUDE.md).

---

## 5. The non-negotiable invariant: identical output

Because this is a refactor, the corpus is the oracle. After the change:

- Every `examples/*.ll` still parses (`ExamplesSpec`).
- The golden `.hs` under `docs/{gcd,prime,safe_div}` are **unchanged** — not
  "regenerated," *unchanged*. If a single byte moves, the refactor altered
  behaviour and is wrong (this is T1 made operational).
- The differential harness (`test/differential/run.py`) still certifies the
  whole corpus bit-for-bit against clang, with no change to the harness itself.

If all three hold, T1/T2 hold on the corpus and the Haskell path is proven
behaviour-preserving.

---

## 6. How this is checked — verifying a refactor with no visible feature

The hard part of evaluating 04 is that "it works" looks identical to "it does
nothing." The verification therefore targets the two theorems directly, plus the
new seam:

1. **T1, by golden diff (regression).** `stack test` + the unchanged
   `docs/*` goldens + `git diff` showing zero change to generated `.hs`. This is
   the primary gate.
2. **T1, as a property test** (new `test/AnnotatedSpec.hs`): for every example,
   `printProgram (annotate (translate ast)) == printProgram (translate ast)`
   after erasure — i.e. relabelling the tree (`fmap (const Pure)` then anything)
   does not change Haskell output. Encodes naturality, not just one corpus run.
3. **T2, pipeline factorisation** (same spec): assert the new
   `render Haskell ∘ annotate ∘ translate` equals the old `printProgram ∘
   translate` on the corpus, so inserting the pass is provably inert on the
   Haskell backend.
4. **The seam is real, not theoretical** (same spec): the `AnnotDump` backend,
   run on a sample program, **reads** the annotation the Haskell backend ignores
   and emits *different* text from the Haskell backend for the *same* tree. A
   second backend that demonstrably consumes `a` is the concrete proof that
   "`PrintAnf` is one of several" is now true, and the executable evidence that
   05/06 have a working landing pad.
5. **Effect lattice laws** (same spec): `Semigroup`/`Monoid` associativity and
   identity for `Effect`, so 05 builds on a lawful join-semilattice.
6. **Build hygiene:** `stack build` warning-clean under the `-Wall` set
   (`package.yaml`), including the `DeriveFunctor`/`DeriveFoldable` additions.

Run targets: `stack test --test-arguments '--match "/annotated/"'`,
`stack test --test-arguments '--match "parses all examples"'`,
`python3 test/differential/run.py`.

---

## 7. How 05 and 06 drop in — the forward-compatibility contract

The deliverable promise (mirroring the [09 plan §7](09-floating-point.md)) is
that the two godmode items become *body* changes, not architecture changes:

- **05 (effect inference)** replaces the body of `annotate` (§4.5) with the real
  bottom-up analysis. It changes **no signatures, no pipeline wiring, no
  backend**. It enriches `data Effect` beyond `Pure` and computes the join with
  `foldMap`. The "every function is `Pure`" baseline is *already running and
  tested* (§6.3/§6.5), so 05 starts from a green, wired pipeline and only has to
  make the labels true. Its sanity milestone ("all pure") is literally the 04
  default.
- **06 (monadic translation)** adds one `instance Backend Monadic` in a new
  module and selects it with `--backend monadic`. It reads the `Effect` label
  (present since 04, meaningful since 05) to choose `do`/`>>=` vs `let … in` per
  block. It changes **nothing** in `Anf`, `Translate`, `Effect`, or the Haskell
  backend — the open/closed property from §2.3.

That both items reduce to "swap one body" / "add one instance" is the test that
04 drew the structural/derived and term/algebra boundaries correctly. If either
needs to touch `Anf`'s structure or `Translate`, 04 mis-placed a boundary.

---

## 8. How the value reaches the *results* (the paper)

04 produces no new translated programs, so its contribution to the dissertation
is **conceptual scaffolding**, and it should be written up as such:

- **A cleaner statement of `F`.** With the annotation parameter, the paper can
  present the translation as producing a *decorated* ANF term and the printers as
  *algebras* over it (§2.3), then state effect analysis as a comonadic relabel
  (§2.1) and monadic emission as a second algebra (§2.4). This is the precise
  vocabulary the Discussion section's "ANF ≡ Moggi's computational λ-calculus"
  claim (cited in [06](../06-monadic-effects-translation.md)) needs to become a
  construction rather than a remark.
- **Two provable refactor theorems (T1/T2, §2.4)** give the paper a worked
  example of *behaviour-preserving program transformation verified by golden +
  property tests* — a small but honest methodological contribution, and the model
  for how the riskier 05/06 changes will later be validated.
- **The figure that pays off later.** A single diagram —
  `Ast → translate → Anf() → annotate → Anf(Effect) → {Haskell | Monadic | …}` —
  is the architecture the Future Work chapter already gestures at; landing 04
  lets the paper draw it as *implemented*, with 05 and 06 shown as the two
  pending bodies/instances rather than open-ended research.

So the value is realised not as a corpus row but as: (a) the formal framing 05/06
are stated in, (b) a verified-refactor methodology exemplar, and (c) a concrete
architecture diagram. The honest framing in the paper is "this chapter builds the
seam; the next two chapters fill it" — which matches the roadmap's "enabler, not
valuable on its own."

---

## 9. Open decisions for the author

1. **`Effect` location.** Ship the trivial `Effect`/`annotate` in 04 (recommended
   — makes T2 testable now, D4), or leave the slot polymorphic and let 05
   introduce `Effect` entirely? Shipping the trivial carrier costs ~3 lines and
   buys a wired, green pipeline; recommended.
2. **Second backend choice.** `AnnotDump` (debug, reads `a`) is the cheapest way
   to *prove* the seam (§6.4). Alternatives that also prove it: a minimal C
   backend, or an S-expression dump of the typed+annotated tree. Pick whichever
   best serves the paper's figure; `AnnotDump` is enough for verification.
3. **`recursion-schemes` / `Cofree`.** This plan deliberately avoids them (§2.1),
   realising the same structure with `DeriveFunctor`/`DeriveFoldable` to match
   the `Ast` idiom and keep the code readable for the TCC. Revisit only if 05's
   analysis turns out to want generic recursion schemes — unlikely at this scope.
4. **Granularity of the effect label.** Per-binding *and* per-block *and*
   per-function (uniform, D1) vs. per-block only. Uniform is recommended; 05 can
   ignore the finer labels if it only needs per-block, and the `Foldable` join
   reconstructs coarser labels from finer ones for free.
