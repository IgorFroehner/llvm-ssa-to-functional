-- | The annotation payload carried by an analysed 'Anf.Program', and the
-- (currently trivial) pass that computes it.
--
-- Effects form a bounded join-semilattice @(E, ⊔, ⊋)@: @⊋ = 'Pure'@ is the
-- bottom (no effect), and the join @e₁ ⊔ e₂@ is "exhibits both effects." The
-- 'Semigroup'\/'Monoid' instances realise @(⊔, ⊥)@ so that a coarser label can
-- be rolled up from finer ones with 'foldMap' — a block's effect is the join of
-- its bindings', a function's the join of its blocks' — straight off the
-- 'Foldable' instance of "Anf".
--
-- The accepted LLVM-IR subset is pure by construction, so the only inhabited
-- point today is 'Pure' and 'annotate' labels every node with it. That is the
-- sanity baseline of [effect inference](docs/roadmap/05-effect-inference.md):
-- every function /must/ infer effect-free. Item 05 replaces the /body/ of
-- 'annotate' (and enriches 'Effect') without changing any signature or the
-- pipeline; see docs/roadmap/plans/04-annotated-anf-ast.md §7.
module Effect
  ( Effect(..)
  , annotate
  ) where

import qualified Anf

-- | The effect lattice. Only 'Pure' is reachable in the current pure subset;
-- item 05 adds the inhabited points (e.g. a call to an unknown declared
-- function, or a trapping division).
data Effect
  = Pure  -- ^ The bottom @⊋@: no observable effect.
  deriving (Eq, Show)

-- | Join @(⊔)@. With only 'Pure' present it is constant, but the law structure
-- is what item 05 builds on, so it is stated now.
instance Semigroup Effect where
  Pure <> Pure = Pure

-- | Identity element @⊋ = 'Pure'@.
instance Monoid Effect where
  mempty = Pure

-- | Decorate a freshly-translated term with effect labels. Today this is the
-- constant-'Pure' relabelling (the pure baseline); item 05 swaps in the real
-- bottom-up inference here. Running it unconditionally — even while trivial —
-- is what keeps the rest of the pipeline (and the backends) stable across that
-- change (plan §3 D4).
annotate :: Anf.Program () -> Anf.Program Effect
annotate = fmap (const Pure)
