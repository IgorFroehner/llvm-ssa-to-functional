---
type: plan
title: Implementation plan — floating-point support and the type discipline it forces
tracks: 09-floating-point.md
status: done
---

## Outcome

Landed. A `src/TypeSystem.hs` module now carries the `Ty` lattice
(`TyInt`/`TyFloat`/`TyDouble`/`TyBool`/`TyUnit`), the elaboration `elaborate`
(total, errors off-subset — replacing the old silent `"Int"` fallback) and the
representation map `rho`. `Anf` carries `Ty` instead of `String`; `ConvOp` carries
source/target `Ty`; the comparison node carries its operand `Ty`; `Value` gained
`FConst`. The `f*` arithmetic (`fadd`/`fsub`/`fmul`/`fdiv`), `fcmp` (ordered +
unordered predicates, **NaN-faithful** via `isNaN` guards — §5), the int↔float /
float↔float conversions, float literals (decimal-exponent form) and the
`nneg`/fast-math flag skips are all in. `TyBool` is wired but dormant (i1 still
elaborates to `TyInt 1`), leaving #10 a localized switch as designed (§7).

The six research-classic examples (`rump`, `muller`, `twosum`, `kahan_sum`,
`newton_sqrt`, `logistic`) plus the type-system/codegen unit specs in
`test/FloatingSpec.hs` are green, and the differential harness — extended for
float signatures, **bit-pattern** comparison and **NaN/±Inf** input sampling
(with NaN-result canonicalisation) — certifies the whole corpus (26 functions)
bit-for-bit exact against native clang, integer corpus included.

Two review-driven corrections after the first cut: (1) `frem` was *removed* from
the subset rather than mapped to `Data.Fixed.mod'`, whose floor semantics differ
from C `fmod` (§11.1); (2) the unordered `fcmp` predicates and `one` were made
NaN-faithful with `isNaN` guards instead of bare operators, which diverged on NaN
inputs (the merged icmp/fcmp node now carries its operand `Ty` so integer `ugt`
stays a plain `>` while float `ugt` is guarded).

Deferred (matches §11 open decisions): hex IEEE float literals (corpus is all
decimal), `ord`/`uno` NaN-test predicates, and the standalone `validate` pass
(types are recoverable locally, so a global Γ was not needed for correctness).
A pre-existing lexer gap surfaced and was fixed: named block labels with dots
(`for.cond.cleanup:`) now lex (the integer corpus only used numeric labels).
---

# Floating point — implementation plan

Tracks [`../09-floating-point.md`](../09-floating-point.md). Goal: accept the
*pure* floating-point fragment of LLVM-IR (`float`/`double` and the `f*`
instruction family) and translate it faithfully into Haskell `Float`/`Double`.

This is **not** a table-rows job like [`03-broader-subset`](03-broader-subset.md).
Floating point is the first feature that breaks the invariant that *every value
is an integer*, an assumption baked into `Translate`, `PrintAnf`, and the
string-typed `Anf` AST. Removing it requires giving the compiler a real, if
small, **type system**: a type lattice, an environment, an elaboration +
validation pass, and a coercion calculus at the boundaries between
representations. The whole point of this plan is to introduce that machinery
*once*, correctly, and in a shape that the next item —
[`10-boolean-types`](../10-boolean-types.md) — drops into with almost no new
code. Because the paper will need to model this mathematically, the plan states
the type system formally (§2–§4) before the engineering (§5 onward).

---

## 1. Where the current design stops being enough

Today "types" are threaded as **bare `String`s** and resolved *locally* at each
binding:

- `Anf.Decl` carries the result type as a `String` (`"Int32"`), and
  `Anf.ConvOp` carries two `Int` widths. There are no other type slots.
- `TranslateAux.hsTypeOfLlvm :: String -> String` maps an LLVM spelling to a
  Haskell type spelling, and — critically — **silently defaults anything it does
  not recognise to `"Int"`** (`src/TranslateAux.hs:105`). That is sound only
  because the accepted subset is integer-only; the moment `float` enters, that
  default is a *miscompile*, not an error.
- `PrintAnf` hard-codes integer assumptions: `icmp` renders as
  `if a < b then 1 else 0`, `select`'s condition as `a /= 0`, branch conditions
  as `v /= 0`, and binary operators come from a type-blind
  `translateOperator :: String -> String` (`add`→`+`, `sdiv`→`` `quot` ``).

[`02-bit-width-fidelity`](02-bit-width-fidelity.md) got away with local,
string-level typing because **LLVM-IR is already a fully, explicitly typed IR**:
every instruction re-annotates the type of its operands and result at the use
site (`%z = add i32 %x, %y`). We never needed to *infer* anything; we read what
LLVM wrote. That observation survives into this plan and shapes the whole
approach — see §2.

What changes is that the *target* representation is no longer a single sort.
Once values can be `IntK`, `Float`, or `Double` (and, in #10, `Bool`),
operator spelling, comparison spelling, the condition forms, and — above all —
the conversions at type boundaries all become **type-directed**. A `String`
cannot direct them; a `Ty` can.

---

## 2. Theoretical foundation: elaboration and validation, not inference

LLVM-IR in the accepted subset is a **monomorphic, explicitly typed** language.
Every SSA value has exactly one statically-known type, written at its definition
and repeated at every use. Two consequences drive the design:

1. **We do not run Hindley–Milner.** There are no type variables to solve. The
   "type system" is an *elaboration* (read LLVM's annotations into an internal
   datatype) composed with a *validation* (check each instruction against
   LLVM's typing rules). Validation is what upgrades today's silent `"Int"`
   fallback into a total function with explicit out-of-subset errors — a
   soundness improvement independent of floats.

2. **Type information is recoverable per instruction**, but a handful of
   positions in LLVM elide it: the condition of `br i1 %c, …` does not restate
   `%c`'s type, nor does `select`'s use of its condition, nor a φ-incoming value
   referenced from a predecessor. To type those uniformly we build a small
   **typing environment** Γ once per function. SSA guarantees each name is
   assigned exactly once, so Γ is a total function on the function's value names
   and can be built by a single pass over all definitions.

### 2.1 The type lattice

Introduce a first-class type, replacing the `String`s:

```haskell
-- src/TypeSystem.hs  (new module)
data Ty
  = TyInt  Int   -- iN; carries the *LLVM* width N (1..64 in subset); rep rounding deferred to ρ
  | TyFloat      -- LLVM `float`  — IEEE-754 binary32 — Haskell Float
  | TyDouble     -- LLVM `double` — IEEE-754 binary64 — Haskell Double
  | TyBool       -- i1-as-Bool. Wired in now, *dormant* until #10 (see §7).
  | TyUnit       -- LLVM `void`
  deriving (Eq, Show)
```

Carrying the *LLVM* width `N` (not the rounded-up representation width) keeps the
type faithful to the source and defers the rounding decision to ρ, exactly where
[`02-bit-width-fidelity`](02-bit-width-fidelity.md) already puts it. We also
introduce the notion of a **sort** (integer / floating / boolean / unit) used by
the validation rules:

> sort(TyInt _) = ℤ  · sort(TyFloat)=sort(TyDouble) = 𝔽 · sort(TyBool)=𝔹 · sort(TyUnit)=𝟙

### 2.2 The representation map ρ : Ty → Haskell

ρ is the generalisation of today's `hsTypeOfLlvm` ∘ `widthToHsType`. It is
**total over the lattice** and is the *only* place a `Ty` becomes a Haskell type
string:

| τ            | ρ(τ)                              | note |
|--------------|-----------------------------------|------|
| `TyInt N`    | `IntK`, K = roundUp(N) ∈ {8,16,32,64} | existing #02 map; N>64 ⇒ error |
| `TyFloat`    | `Float`                           | IEEE binary32 |
| `TyDouble`   | `Double`                          | IEEE binary64 |
| `TyBool`     | `Bool`                            | dormant until #10 |
| `TyUnit`     | `()`                              | existing void→unit (#08) |

### 2.3 Elaboration ⌈·⌉ : LLVM type spelling → Ty

A total function `elaborate :: String -> Ty` that *fails loudly* off-subset
(replacing the `"Int"` default):

```
⌈"iN"⌉   = TyInt N           ⌈"float"⌉  = TyFloat
⌈"void"⌉ = TyUnit            ⌈"double"⌉ = TyDouble
⌈"half" | "fp128" | "ptr" | …⌉ = error "out of subset: <t>"
```

Note `i1` still elaborates to `TyInt 1` under this plan, **not** `TyBool` — the
i1→Bool refinement is #10's single, localized switch (§7).

---

## 3. The typing judgement for the ANF metalanguage

For the paper we give a declarative type system for the emitted ANF fragment and
prove the translation lands inside it. Let Γ range over finite maps from value
names to `Ty`. Constants are typed against an expected type (LLVM constants are
untyped literals given meaning by their context, exactly as in LLVM-IR), so the
judgement for values is bidirectional in the constant case:

```
─────────────────  (T-Var)        ─────────────────────  (T-IConst)
Γ ⊢ x : Γ(x)                       Γ ⊢ (n : ℤ) ⇐ TyInt N

────────────────────  (T-FConst)        ───────────────────  (T-Unit)
Γ ⊢ (r : ℝ) ⇐ TyFloat|TyDouble          Γ ⊢ () : TyUnit
```

Instruction rules (⊕ ranges over arithmetic binops, ⋈ over comparisons):

```
 Γ⊢a:τ   Γ⊢b:τ   sort(τ)∈{ℤ,𝔽}            Γ⊢a:τ  Γ⊢b:τ   sort(τ)∈{ℤ,𝔽}
 ─────────────────────────────  (T-Bin)   ───────────────────────────────  (T-Cmp)
        Γ ⊢ a ⊕τ b : τ                          Γ ⊢ a ⋈τ b : TyInt 1†

 Γ⊢c:κ  Γ⊢a:τ  Γ⊢b:τ   κ∈{TyInt 1,TyBool}      Γ ⊢ a : σ    coerce σ τ defined
 ──────────────────────────────────────  (T-Sel)  ──────────────────────────────  (T-Conv)
        Γ ⊢ select c a b : τ                          Γ ⊢ ⟨σ→τ⟩ a : τ
```

† Under #10 the result of T-Cmp becomes `TyBool`; that is the *only* rule that
changes. (See §7.)

The φ/control rules tie blocks together: a block translated as a λ has parameter
types given by its φ-nodes' declared types, and every branch into it is a
tail-call whose argument types must match those parameters — this is precisely
the SSA→ANF correspondence the project already implements structurally, now
*typed*:

```
 block B has φ-params (p₁:τ₁ … pₖ:τₖ)     each predecessor P supplies
 a tail call B(v₁…vₖ) with Γ_P ⊢ vᵢ : τᵢ
 ────────────────────────────────────────────────────────────────────  (T-Phi/Br)
                 λ(p₁…pₖ). … : τ₁ → … → τₖ → τ_ret
```

### 3.1 Type-preservation theorem

> **Theorem (F is type-preserving).** Let `f` be an LLVM-IR function in the
> accepted subset, well-typed under LLVM's own type system with signature
> `τ₁,…,τₙ → τ_r`. Then `F(f)` is a closed ANF term and
> `⊢ F(f) : ρ(⌈τ₁⌉) → … → ρ(⌈τₙ⌉) → ρ(⌈τ_r⌉)`.

*Proof sketch.* Structural induction over the dominator-tree walk
(`anfFromTree`). Each LLVM instruction is well-typed under LLVM's rules by
hypothesis; `elaborate` maps its annotated operand/result types into `Ty`, and
each translation clause (`anfBinOp`, `anfIcmp`, `anfConvOp`, …) produces exactly
the ANF former whose typing rule above has matching premises — the *validation*
pass (§2) is what discharges those premises before emission. φ-parameter and
tail-call types coincide by (T-Phi/Br) because the project resolves a branch's
arguments from the *target* block's φ-nodes (`getValueForCurrentLabel`), whose
declared types are the λ's parameter types. The representation map ρ is applied
uniformly at print time, so the emitted Haskell signature is
ρ ∘ ⌈·⌉ of the LLVM signature. ∎

### 3.2 Semantic adequacy and the IEEE caveats

Type preservation is the structural half; for the paper we also state the
semantic half and its honest boundaries:

> **Adequacy (informal).** For inputs in the representable domain,
> `⟦F(f)⟧ = ⟦f⟧`.

For the integer fragment this is the bit-width result already certified by the
differential harness. For the floating fragment it rests on three facts that
must be stated as *assumptions*, because they are where faithfulness can leak:

1. **Format match.** Haskell `Float`/`Double` are IEEE-754 binary32/binary64,
   the same formats as LLVM `float`/`double`; GHC lowers `+ - * /` to the
   platform FPU, as clang does. So `fadd`/`fsub`/`fmul`/`fdiv` agree bit-for-bit
   under the default rounding mode (round-to-nearest-even).
2. **No fast-math.** LLVM `fast`/`nnan`/`ninf`/`reassoc` flags license
   non-IEEE rearrangement. We **skip** these flags in the lexer (as we already
   skip `nsw`), and the subset is therefore the *strict-FP* fragment. This is a
   documented limitation, not a bug.
3. **Conversion rounding direction.** `fptosi`/`fptoui` are
   **round-toward-zero** in LLVM, so they map to Haskell `truncate`, *never*
   `round`. (§4.) Getting this wrong is the classic floating-conversion
   miscompile.

`frem` is C's `fmod` (not Haskell's `Prelude.mod`); it maps to
`Foreign.C` style `c_fmod` semantics, available as
`Data.Fixed.mod'` for `RealFrac` — verify bit-exactness in the harness or
declare `frem` out of scope if it diverges (open decision, §11).

---

## 4. The coercion calculus — the one mechanism that serves floats *and* Bool

Every place two representations meet is mediated by a single partial function

```
coerce :: Ty -> Ty -> (Value -> String)      -- source Ty, target Ty, Haskell text
```

The LLVM conversion instructions are *names for entries in this table*; so is the
implicit i1↔int coercion that #10 will need. Defining the table once is what
makes Bool a near-free follow-up.

| LLVM conv | σ → τ                | `coerce σ τ x` (Haskell)                                  | semantic note |
|-----------|----------------------|-----------------------------------------------------------|---------------|
| `trunc`   | `iN → iM` (N>M)      | `fromIntegral x :: IntM`                                  | wraps (existing #02) |
| `zext`    | `iN → iM`            | `fromIntegral (fromIntegral x :: WordN) :: IntM`          | zero-extend (existing #02) |
| `sext`    | `iN → iM`            | `fromIntegral x :: IntM`                                  | sign-extend (existing #02) |
| `sitofp`  | `iN → fp`           | `fromIntegral x :: ρ(fp)`                                 | signed int→float |
| `uitofp`  | `iN → fp`           | `fromIntegral (fromIntegral x :: WordN) :: ρ(fp)`         | unsigned int→float |
| `fptosi`  | `fp → iN`           | `truncate x :: IntN`                                     | **round toward zero** |
| `fptoui`  | `fp → iN`           | `fromIntegral (truncate x :: WordN) :: IntN`            | toward zero, via Word |
| `fpext`   | `float → double`     | `GHC.Float.float2Double x`                               | exact widen |
| `fptrunc` | `double → float`     | `GHC.Float.double2Float x`                               | rounds |
| *(#10)*   | `i1/Bool → iN`       | `if x then 1 else 0 :: IntN`  (`zext i1`)                | Bool→int |
| *(#10)*   | `iN → Bool`          | `x /= 0`                                                  | int→Bool (cond use) |

Notes that must not be gotten wrong:
- Use `GHC.Float.float2Double` / `double2Float` rather than `realToFrac` for
  `fpext`/`fptrunc`: `realToFrac` routes through `Rational` on some GHCs and is
  lossy/odd around NaN; the `GHC.Float` primitives are the bit-exact lowering.
- `coerce` is **partial**; an undefined pair (e.g. `double → double` requested,
  or a sortless conversion) is a validation error, surfacing malformed input
  instead of emitting garbage.

The arithmetic/comparison spellings likewise become **type-indexed**:

```
op⊕ :: Ty -> String      -- add↦"+", sdiv↦"`quot`", fdiv↦"/", fadd↦"+", frem↦"`mod'`", …
op⋈ :: Ty -> String      -- icmp slt↦"<"; fcmp olt↦"<"; fcmp one↦"/="; …
```

For `+ - *` the spelling coincides across ℤ and 𝔽 (Haskell `Num`), but division,
remainder, and *every comparison* differ, and the `f*` mnemonics are distinct
tokens — so the dispatch must key on `Ty`, not on the bare mnemonic.

---

## 5. The LLVM floating surface to support

Inventory, with the typing rule each must satisfy (validated per §2):

- **Arith:** `fadd fsub fmul fdiv frem` — `fp → fp → fp` (T-Bin, sort 𝔽).
- **Compare:** `fcmp <pred>` — `fp → fp → i1` (T-Cmp). Ordered/unordered
  predicates: `oeq ogt oge olt ole one ord` and `ueq ugt uge ult ule une uno`.
  **The unordered set is not optional.** The corpus (`newton_sqrt.ll`) shows
  clang lowering an ordinary C `a <= 0.0` guard to `fcmp ugt` — i.e. clang
  routinely picks the *unordered* predicate so that a NaN operand falls through
  to the negated branch (`!(a > 0)` must hold when `a` is NaN). So the minimum
  viable predicate set must include the unordered comparisons, not just the
  ordered ones. **They must be NaN-faithful, not merely "right on the no-NaN
  domain":** an unordered predicate is `True` whenever an operand is NaN, but
  Haskell's `<`/`>`/`==` return `False` on NaN, so each unordered relational and
  `ueq` is emitted with an explicit `isNaN` guard
  (`ugt` → `isNaN a || isNaN b || a > b`), and the ordered `one` (false unless
  both operands are ordered) as `a == a && b == b && a /= b`. The ordered
  relationals (`o*`) and `une` already coincide with the bare Haskell operators.
  This is why the comparison node carries its *operand* `Ty` (§6.2): the same
  spelling `ugt` is an unsigned-integer compare under `icmp` (plain `>`) and an
  unordered float compare under `fcmp` (`isNaN`-guarded), and only the operand
  sort tells them apart. `ord`/`uno` (explicit `x == x` / `x /= x` NaN tests)
  remain the §11 open decision, since nothing in the corpus emits them.
- **Int↔float conversions:** `sitofp uitofp fptosi fptoui` (§4).
- **Float↔float conversions:** `fpext fptrunc` (§4).
- **Literals:** `float`/`double` constants in LLVM's decimal-exponent form
  (`1.500000e+00`) and hexadecimal IEEE form (`double 0x4008000000000000`).
  See §6.4 — this is its own sub-task.

Out of scope (state explicitly): `half`/`fp128`, vector FP, FP intrinsics
(`llvm.sqrt`, `llvm.fma`, …) beyond what the corpus needs, and all fast-math
fragments.

---

## 6. Implementation, file by file

The unifying move: **replace the `String` type slots in `Anf` with `Ty`**, route
all of `Translate` through `elaborate`, and make `TranslateAux`/`PrintAnf`
type-directed. This is also a down payment on
[`04-annotated-anf-ast`](../04-annotated-anf-ast.md) (typed annotation slots) —
worth calling out in the paper as the refactor that item formalises.

### 6.1 New module `src/TypeSystem.hs`

Home of the §2–§4 machinery, kept separate so `Translate` stays a syntax-directed
walk and the theory has a 1:1 code home:

- `data Ty` (§2.1) and `data Sort`; `sortOf :: Ty -> Sort`.
- `elaborate :: String -> Ty` (§2.3), total, errors off-subset.
- `rho :: Ty -> String` (§2.2) — supersedes `hsTypeOfLlvm`/`widthToHsType`
  (move those here or re-express them through `rho`).
- `coerce :: Ty -> Ty -> Value -> String` (§4), partial with explicit errors.
- `opArith :: Ty -> String -> String`, `opCmp :: Ty -> String -> String`
  (type-indexed §4 spellings) — supersede `translateOperator`/`translateCmpType`.
- `type Gamma = Data.Map.Map String Ty`; `buildGamma :: Ast.Function Range ->
  Gamma` (one pass over every definition: args, φ-nodes, and each `Dec`'s
  result), plus `validate :: Gamma -> Ast.Function Range -> Either TypeError ()`
  applying the §3 rules. `validate` is run by `translate` before emission; a
  `Left` aborts with a clear message (the soundness upgrade over the silent
  `"Int"`).

### 6.2 `src/Anf.hs` — types become `Ty`, not `String`

- Replace the result-type `String` on `DeclBinOp/DeclCall/DeclIcmp/DeclSelect/
  DeclFreeze` with `Ty`.
- Generalise `ConvOp String Int Int Value` to `ConvOp ConvKind Ty Ty Value`
  carrying **source and target `Ty`** (not widths) — conversions now cross sorts,
  so widths alone are insufficient. (`ConvKind` = the LLVM op name or an enum.)
- The `Function` arg-types/return-type fields become `[Ty]` / `Ty`.
- Add a floating constant to `Value`: `FConst Rational Ty` (keep the precise
  value as `Rational` plus the intended `Ty` so the printer chooses `Float` vs
  `Double` and a round-tripping literal). `Const Integer` stays for ints.

### 6.3 `src/Lexer.x`

- `float`/`double` already lex as `Type` (`src/Lexer.x:92`) — good, no change for
  the type tokens.
- Add the `f*` arithmetic mnemonics as `BinOp` tokens: `fadd fsub fmul fdiv frem`.
- Add `fcmp` (its own token, parallel to `icmp`) and its predicate set to the
  `Cmp` rule (extend the alternation at `src/Lexer.x:101`). The set **must
  include the unordered predicates** (`ueq ugt uge ult ule une`), not just the
  ordered ones: `newton_sqrt.ll` already emits `fcmp ugt` for a plain `a <= 0.0`
  guard (see §5). The existing `Cmp` token spellings (`ugt ult …`) are reused by
  the integer `icmp` and FP `fcmp` alike — they only differ in operand sort,
  which the type system already tracks.
- Add the FP conversions as `ConvOp` tokens: `sitofp uitofp fptosi fptoui fpext
  fptrunc`.
- **Skip `nneg`.** Clang tags `uitofp`/`zext` results that are provably
  non-negative with the `nneg` flag (`kahan_sum.ll` emits `uitofp nneg i32 …`);
  it is a non-semantic hint, so drop it in the lexer exactly as `nsw`/`nuw` are
  already dropped — otherwise it lands between the op and its type and breaks the
  conv-op production.
- **Float literals** (§6.4): a new token rule for decimal-exponent and `0x…`
  hex IEEE forms. Skip fast-math flag keywords (`fast nnan ninf nsz arcp
  contract reassoc afn`) the way `nsw`/`nuw` are already dropped. (The corpus is
  compiled `-ffp-contract=off`, so none of these appear in it — but a hand-written
  or differently-compiled `.ll` may carry them.)

### 6.4 Float literals — its own careful sub-task

LLVM prints FP constants two ways:
- **Decimal/exponential:** `1.500000e+00`, `-3.000000e+00`.
- **Hex IEEE bit-pattern:** `0x4008000000000000` (and the `0xK/L/M/H` tagged
  forms for `x86_fp80`/`fp128`/`half` — out of subset, error on those tags).

Plan:
- Lexer produces the raw lexeme; a parser/elaborator helper `parseFpLit ::
  Ty -> String -> Rational` interprets it: decimal via `read`/`readFloat`; the
  `0x…` form by reading 64 bits and `castWord64ToDouble`
  (`GHC.Float`/`Data.Word`), then to `Rational`. For `float` literals LLVM still
  prints the value widened to the `double` hex; convert to `Float` then store.
- `value` production (`src/Parser.y:150`) gains a `fpLit` alternative; `Ast.Value`
  gains a floating constructor mirroring `IntegerValue`.
- Printing: emit a Haskell literal that round-trips bit-exactly. `show (d ::
  Double)` is round-trip-faithful in GHC (shortest decimal s.t. `read . show =
  id`), so emit `show v :: Double` / `:: Float`. Verify in the harness via
  bit-pattern comparison (§8), not decimal eyeballing.

### 6.5 `src/Parser.y`

- Token decls for the new lexer tokens; `fcmp` production paralleling `icmp`
  (`src/Parser.y:193`) building an `Ast.Fcmp` (or reuse `Icmp` with a flag — but
  a distinct node keeps the validation rules clean; recommend a sibling node).
- `binOpCall` already generic over the `BinOp` token, so `f*` arithmetic needs no
  new production — only the lexer tokens and the type-indexed spelling in
  `opArith`.
- `convOperation` (`src/Parser.y:212`) already carries both source and target
  `typeAnotation`; the FP conversions parse with no grammar change — only new
  `ConvOp` tokens.
- `value`/`Ast.Value` extended for FP literals (§6.4).

### 6.6 `src/Translate.hs`

- Run `buildGamma` + `validate` (§6.1) at the top of `translateFunction`;
  thread `Gamma` into the block walk so condition/φ-incoming uses can be typed.
- Replace every `typeStr`/`hsTypeOfLlvm` call with `elaborate` producing a `Ty`
  that is stored in the `Anf` nodes (no longer a `String`).
- `anfConvOp`: build `Anf.ConvOp` from the two elaborated `Ty`s (and the op kind)
  instead of two widths.
- `anfIcmp`/new `anfFcmp`: the result type stays `TyInt 1` for #09 (dormant Bool
  seam, §7).
- Float-constant operands flow through `anfValue` into the new `Value`
  constructor.

### 6.7 `src/TranslateAux.hs`

- Demote `translateOperator`/`translateCmpType`/`hsTypeOfLlvm`/`widthToHsType`/
  `widthToWordType` in favour of the `TypeSystem` exports (`opArith`/`opCmp`/
  `rho`/`coerce`). Keep `widthToWordType` as a helper used by `coerce`.

### 6.8 `src/PrintAnf.hs`

- `annot`/`declString` consume `Ty` and call `rho`.
- `printConvOp` becomes a thin call to `coerce src tgt value` (it already
  special-cases `zext`; that logic moves into the `coerce` table).
- `printBinOp` keys the operator on the operand `Ty` via `opArith` (the operands'
  `Ty` is available from the decl's result `Ty` for same-sort binops; for safety
  consult `Gamma` — pass the operand `Ty` into the node at translate time so the
  printer stays Γ-free). The shift-amount `fromIntegral` coercion stays for the
  integer shifts.
- `printIcmp` stays integer (`if a < b then 1 else 0`) for #09; **`printFcmp`**
  emits the same shape with `opCmp TyFloat`/`TyDouble` spelling. (#10 collapses
  both to a bare `Bool` form — §7.)
- Header imports: add `GHC.Float` (for `float2Double`/`double2Float`) and, if
  `frem` is supported, `Data.Fixed`.

---

## 7. How Bool (#10) drops in — the forward-compatibility contract

The deliverable promise of this plan is that
[`10-boolean-types`](../10-boolean-types.md) needs **no new machinery**, only
flips already-built switches. After #09 lands, #10 is exactly:

1. **`elaborate "i1" = TyBool`** instead of `TyInt 1` (one line in §2.3).
2. **T-Cmp result = `TyBool`** — change `anfIcmp`/`anfFcmp` to tag the result
   `TyBool` (the dagger † in §3).
3. **Two `coerce` rows activate** (already tabulated in §4): `Bool→iN` for the
   `zext i1` clang emits, and `iN→Bool` for any residual integer-as-condition.
4. **Condition forms simplify**: `printIcmp`/branch/`select` drop the
   `then 1 else 0` / `/= 0` scaffolding because the condition is already `Bool`
   (these become `rho`-driven: `Bool` conditions print bare, integer conditions
   keep `/= 0`).
5. **ρ(TyBool) = "Bool"** — already in §2.2.

That is the entire #10 diff. Nothing in `Translate`'s structure, the grammar, or
the coercion mechanism changes — which is the test that this plan got the
abstraction boundary right. Keep `TyBool` in the lattice from day one (exported,
so `-Wall` is quiet) precisely so #10 is a refinement, not a refactor.

---

## 8. Differential harness (`test/differential/run.py`)

Today the harness assumes integer signatures: it skips any function whose
signature has a non-int type (`run.py:140`), reads args with `strtoll`, prints
`(long long)` on the C side and `fromIntegral … :: Integer` on the Haskell side
(`run.py:157`, `:196`). Floating support means:

- **Type-directed sampling.** Extend `CTYPE`/sampler maps with `float`/`double`;
  generate finite, non-NaN/non-Inf float samples (and a few edge values: ±0,
  subnormal, large-magnitude) per arg type.
- **Bit-exact comparison, not decimal.** Printing floats as decimal risks
  off-by-an-ULP false mismatches. Compare the **raw IEEE bit pattern** from both
  sides: C prints the `uint64_t`/`uint32_t` reinterpretation of the result
  (`memcpy` + `%llu`/`%u`, or `%a` hex-float); Haskell prints
  `castDoubleToWord64`/`castFloatToWord32` (`GHC.Float`). Equal bit patterns ⇒
  certified exact; this is also NaN-robust (bit-identical NaNs compare equal).
- **Stop skipping FP signatures** once the path exists, so new FP examples are
  certified by the same gate as the integer corpus.

Document in `run.py`'s header that the certification covers the **strict-FP**
fragment (no fast-math), matching §3.2.

---

## 9. Example corpus (build first, TDD)

Per the [`03`](03-broader-subset.md) convention: author each as C, compile with
`clang -O1 -S -emit-llvm -fno-discard-value-names`, stage under
`examples/floating-point/` (the subdir `ExamplesSpec` ignores) while
unsupported, then **promote to `examples/` top-level as each lands** so the parse
gate and differential gate pick it up.

Suggested sources (cover every §5 row):
- `fadd.c` — `double f(double a,double b){return a+b;}` (arith + literals).
- `fdiv.c` — `double g(double a,double b){return a/b;}` (the `/` vs `quot` split).
- `fcmp.c` — `int p(double a,double b){return a<b;}` (fcmp → i1, mixed sorts via
  the implicit `zext`).
- `i2f.c` — `double h(int n){return n + 0.5;}` (sitofp + literal).
- `f2i.c` — `int t(double x){return (int)x;}` (fptosi, round-toward-zero).
- `fext.c` — `double w(float x){return x;}` (fpext); `ftr.c` —
  `float n(double x){return x;}` (fptrunc).
- A small **mixed loop** (φ-carried `double` accumulator) to exercise (T-Phi/Br)
  with a floating parameter type — e.g. a fixed-iteration sum.

---

## 10. Tests (TDD — red first)

- **`test/ExamplesSpec.hs`** — promoting examples extends the parse gate
  automatically.
- **New `test/TypeSystemSpec.hs`** — unit-test the type machinery in isolation
  (it is the risky part): `elaborate` round-trips and errors off-subset; `rho`
  table; `coerce` produces the exact §4 strings for each pair *and is `error`/`Left`
  on undefined pairs; `validate` rejects a hand-built ill-typed function
  (e.g. `fadd` on `i32` operands) — these are the executable form of §3's rules.
- **New `test/FloatingSpec.hs`** — drive the real pipeline
  (`runAlex … parseLLVMIR` → `translate` → `printProgram`) and assert the emitted
  Haskell: `/` for `fdiv`, `+` for `fadd`, `truncate … :: Int32` for `fptosi`,
  `float2Double` for `fpext`, `:: Double` annotations and round-tripping literals,
  and the function signature `f :: Double -> Double -> Double`.
  Run: `stack test --test-arguments '--match "/floating/"'`.
- **`test/differential/run.py`** — every promoted FP example certified
  bit-pattern-exact (§8).

---

## 11. Open decisions for the author

1. **`frem` semantics.** *Resolved: out-of-subset.* `Data.Fixed.mod'` is
   floor-based (result takes the divisor's sign), but `frem`/C `fmod` truncates
   the quotient toward zero (result takes the dividend's sign) — e.g.
   `fmod(-5,3) = -2` vs `mod' (-5) 3 = 1`. No *pure* Haskell primitive matches
   `fmod` bit-exactly (the naive `a - b*truncate(a/b)` carries rounding error),
   so `frem` is rejected rather than mapped wrongly; a faithful version would
   need an FFI `fmod`. It appears nowhere in the corpus.
2. **`ord`/`uno` (explicit NaN tests) only.** *Settled for the rest:* the
   ordered **and** unordered relational predicates are both required — the corpus
   forces it (`newton_sqrt.ll` emits `fcmp ugt`, §5), and on the no-NaN corpus
   they collapse to the same Haskell comparison. The only still-open question is
   whether to implement `ord`/`uno` (the bare `x == x` / `x /= x` NaN tests) or
   error on them; nothing in the corpus emits them, so erroring until needed is
   safe.
3. **`uitofp`/`fptoui`** appear only for unsigned C source; the project's
   signedness limitation (#02) already documents unsigned gaps. Include them in
   `coerce` (cheap) but corpus-test only if a clean example exists.
4. **Γ scope.** Build the full `Gamma` (recommended, enables clean condition
   typing and #10) vs. stay fully local like #02? The plan assumes the former;
   it is the small extra cost that buys the validation soundness and the Bool
   seam.

---

## 12. Verification

1. `stack build` — warning-clean (`-Wall` + the `-W…` set in `package.yaml`);
   edit `package.yaml` (hpack) to register `TypeSystem`/new spec modules, never
   the generated `.cabal`.
2. `stack test` — `ExamplesSpec` parses all (incl. promoted FP examples);
   `TypeSystemSpec`, `FloatingSpec` green; update golden `.hs` in
   `docs/{gcd,prime,safe_div}` only if the `String`→`Ty` refactor changes their
   (integer) output — it should not, which is itself a regression check that the
   refactor preserved the integer path.
3. `python3 test/differential/run.py` — every row certified, integer corpus
   unchanged and new FP rows bit-pattern-exact.
