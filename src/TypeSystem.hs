-- | The small type discipline that lets the translator handle more than one
-- value sort. LLVM-IR is explicitly typed, so this is an /elaboration/ (read
-- LLVM's annotations into 'Ty') plus a /representation map/ ('rho', the only
-- place a 'Ty' becomes a Haskell type), not type inference.
--
-- The lattice is intentionally wider than the integer-only past: 'TyFloat' /
-- 'TyDouble' are introduced here for floating-point support, and 'TyBool' is
-- wired in but dormant (i1 still elaborates to @TyInt 1@) so the i1-as-Bool
-- refinement is a localized follow-up rather than a refactor. See
-- docs/roadmap/plans/09-floating-point.md.
module TypeSystem
  ( Ty(..)
  , elaborate
  , rho
  , widthOf
  ) where

import TranslateAux (widthToHsType, llvmIntWidth)

-- | The internal type lattice. @TyInt@ carries the /LLVM/ width N; the rounding
-- up to a representable @IntK@ is deferred to 'rho'.
data Ty
  = TyInt Int   -- ^ @iN@ (1..64 in subset)
  | TyFloat     -- ^ LLVM @float@  — IEEE-754 binary32 — Haskell 'Float'
  | TyDouble    -- ^ LLVM @double@ — IEEE-754 binary64 — Haskell 'Double'
  | TyBool      -- ^ i1-as-Bool. Dormant until the boolean-types item.
  | TyUnit      -- ^ LLVM @void@
  deriving (Eq, Show)

-- | Elaborate an (already punctuation-stripped) LLVM type spelling into a 'Ty'.
-- Total: anything off-subset is a loud error rather than a silent fallback.
elaborate :: String -> Ty
elaborate "void"   = TyUnit
elaborate "float"  = TyFloat
elaborate "double" = TyDouble
elaborate s        = TyInt (llvmIntWidth s)  -- errors if not an @iN@

-- | The Haskell type a 'Ty' is represented by. Integer widths round /up/ to the
-- next available 'Data.Int' size (handled by 'widthToHsType').
rho :: Ty -> String
rho (TyInt n) = widthToHsType n
rho TyFloat   = "Float"
rho TyDouble  = "Double"
rho TyBool    = "Bool"
rho TyUnit    = "()"

-- | The bit-width of an integer 'Ty'. Partial: only valid at positions the
-- subset guarantees integer (the integer side of a conversion).
widthOf :: Ty -> Int
widthOf (TyInt n) = n
widthOf t         = error ("widthOf: not an integer type: " ++ show t)
