-- | The small type discipline that lets the translator handle more than one
-- value sort. LLVM-IR is explicitly typed, so this is an /elaboration/ (read
-- LLVM's annotations into 'Ty') plus a /representation map/ ('rho', the only
-- place a 'Ty' becomes a Haskell type), not type inference.
--
-- The lattice is intentionally wider than the integer-only past: 'TyFloat' /
-- 'TyDouble' carry floating-point support, and 'TyBool' carries the i1-as-Bool
-- translation: @i1@ elaborates to 'TyBool', so an LLVM comparison result is a
-- Haskell 'Bool' fed straight into 'if', and an @i1@ return type prints @Bool@.
-- See docs/roadmap/plans/09-floating-point.md §7 and
-- docs/roadmap/10-boolean-types.md.
module TypeSystem
  ( Ty(..)
  , elaborate
  , rho
  , widthOf
  , isFloating
  ) where

import TranslateAux (widthToHsType, llvmIntWidth)

-- | The internal type lattice. @TyInt@ carries the /LLVM/ width N; the rounding
-- up to a representable @IntK@ is deferred to 'rho'.
data Ty
  = TyInt Int   -- ^ @iN@ (1..64 in subset)
  | TyFloat     -- ^ LLVM @float@  — IEEE-754 binary32 — Haskell 'Float'
  | TyDouble    -- ^ LLVM @double@ — IEEE-754 binary64 — Haskell 'Double'
  | TyBool      -- ^ @i1@ — IEEE has no part here; this is LLVM's boolean.
  | TyUnit      -- ^ LLVM @void@
  deriving (Eq, Show)

-- | Elaborate an (already punctuation-stripped) LLVM type spelling into a 'Ty'.
-- Total: anything off-subset is a loud error rather than a silent fallback.
elaborate :: String -> Ty
elaborate "void"   = TyUnit
elaborate "float"  = TyFloat
elaborate "double" = TyDouble
elaborate "i1"     = TyBool
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

-- | Whether a 'Ty' is an IEEE floating type. Used to pick NaN-faithful codegen
-- for @fcmp@ (which shares predicate spellings with integer @icmp@).
isFloating :: Ty -> Bool
isFloating TyFloat  = True
isFloating TyDouble = True
isFloating _        = False
