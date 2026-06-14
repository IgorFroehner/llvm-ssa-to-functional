{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

-- | The output (functional / ANF) abstract syntax. Every node is parameterised
-- over an /annotation/ type @a@, exactly as the input 'Ast.Program' is
-- parameterised over a source 'Range'. The annotation is a slot for /derived/
-- analysis results — an effect label, primarily (see "Effect") — that a pass
-- writes onto an already-built tree and a backend may read.
--
-- Two kinds of decoration live on this tree, and they are deliberately kept
-- apart (see docs/roadmap/plans/04-annotated-anf-ast.md §2.1):
--
--   * /Essential/ data — the 'Ty' fields — is part of a term's meaning and
--     directs code generation; every backend consumes it. It is constructor
--     data, not an annotation, and is present from construction.
--   * /Derived/ data — the @a@ annotation — is computed by a pass after the
--     term exists and is consumed by /some/ backends only. The base term built
--     by "Translate" carries @()@; the "Effect" pass relabels it to @Effect@.
--
-- Because @a@ is the last type parameter and is threaded through every recursive
-- position, the derived 'Functor' gives label rewriting (and, with
-- @'fmap' (const ())@, label /erasure/) and the derived 'Foldable' gives label
-- aggregation (@'foldMap'@ joins every label — used to roll a block's effect up
-- from its bindings'). These two come for free precisely because of the uniform
-- parameterisation; nothing hand-written traverses the tree.
module Anf (
  Program(..),
  Function(..),
  ArgumentDef(..),
  Lambda(..),
  Expr(..),
  Decl(..),
  ConvOp(..),
  Select(..),
  Icmp(..),
  Flow(..),
  BinOp(..),
  Call(..),
  IfThenElse(..),
  Value(..),
  erase,
) where

import TypeSystem (Ty)

-- | Erase every annotation, recovering the bare term. By naturality this is the
-- identity as far as any annotation-blind backend (e.g. the Haskell printer) is
-- concerned: @render . erase = render@. See plan §2.4 (T1).
erase :: Functor f => f a -> f ()
erase = fmap (const ())

newtype Program a = Program [Function a]
  deriving (Show, Eq, Functor, Foldable)

-- | A whole LLVM function. The @['Ty']@ are the argument types and the trailing
-- 'Ty' is the return type; together they form the emitted top-level signature,
-- which anchors bit-width inference. The leading @a@ is the function-level
-- annotation (e.g. its aggregate effect).
data Function a =
  Function a String [ArgumentDef a] [Ty] Ty (Lambda a) (Call a)
  deriving (Show, Eq, Functor, Foldable)

data ArgumentDef a =
  ArgumentDef a String
  deriving (Show, Eq, Functor, Foldable)

-- | A basic block, reconstructed as a lambda. The leading @a@ is the block-level
-- annotation — the natural granularity for per-block purity in
-- [effect inference](docs/roadmap/05-effect-inference.md).
data Lambda a =
  Lambda a String [ArgumentDef a] [Expr a] [Lambda a] (Flow a)
  deriving (Show, Eq, Functor, Foldable)

newtype Expr a
  = ExpDecl (Decl a)
  -- | ExpCall (Call a)
  deriving (Show, Eq, Functor, Foldable)

-- | Each binding carries the 'Ty' of its result so the printer can annotate it
-- and pin the representation, and the leading @a@ for its derived label — the
-- finest effect granularity. 'DeclConvOp' has no result-'Ty' field because the
-- conversion already names its target type.
data Decl a
  = DeclBinOp a String Ty (BinOp a)
  | DeclCall a String Ty (Call a)
  | DeclIcmp a String Ty (Icmp a)
  | DeclSelect a String Ty (Select a)
  | DeclConvOp a String (ConvOp a)
  -- | An LLVM @freeze@: a typed alias binding @name = value@ (freeze is the
  -- identity in the pure subset). The 'Ty' is the result type annotation.
  | DeclFreeze a String Ty (Value a)
  deriving (Show, Eq, Functor, Foldable)

-- | A conversion: the LLVM op name (@trunc@\/@zext@\/@sext@\/@sitofp@\/…), the
-- source and target 'Ty' (conversions may cross sorts, so widths alone are
-- insufficient), and the value being converted.
data ConvOp a
  = ConvOp a String Ty Ty (Value a)
  deriving (Show, Eq, Functor, Foldable)

data Select a
  = Select a (Value a) (Value a) (Value a)
  deriving (Show, Eq, Functor, Foldable)

-- | A comparison: the predicate, the /operand/ 'Ty' (so the printer can pick
-- NaN-faithful codegen for floating @fcmp@ vs a plain operator for integer
-- @icmp@), and the two operands. The result is always an i1.
data Icmp a
  = Icmp a String Ty (Value a) (Value a)
  deriving (Show, Eq, Functor, Foldable)

data Flow a
  = FlowCall (Call a)
  | FlowCond (IfThenElse a)
  deriving (Show, Eq, Functor, Foldable)

data BinOp a
  = BinOp a String (Value a) (Value a)
  deriving (Show, Eq, Functor, Foldable)

data Call a
  = Call a (Value a) [Value a]
  deriving (Show, Eq, Functor, Foldable)

data IfThenElse a
  = IfThenElse a (Value a) (Call a) (Call a)
  deriving (Show, Eq, Functor, Foldable)

data Value a
  = Const a Integer
  -- | A floating constant: the (Haskell-renderable) literal text and its 'Ty'
  -- (so the printer emits @Float@ vs @Double@ and an explicitly-typed literal).
  | FConst a String Ty
  -- | A boolean constant, from an @i1@ literal (LLVM @true@\/@false@). Printed
  -- bare as @True@\/@False@.
  | BConst a Bool
  | Name a String
  | Unit a
  deriving (Show, Eq, Functor, Foldable)
