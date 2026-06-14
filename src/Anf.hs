
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
) where

import TypeSystem (Ty)

newtype Program = Program [Function] deriving (Show, Eq)

-- | A whole LLVM function. The @['Ty']@ are the argument types and the trailing
-- 'Ty' is the return type; together they form the emitted top-level signature,
-- which anchors bit-width inference.
data Function =
  Function String [ArgumentDef] [Ty] Ty Lambda Call
  deriving (Show, Eq)

newtype ArgumentDef =
  ArgumentDef String
  deriving (Show, Eq)

data Lambda =
  Lambda String [ArgumentDef] [Expr] [Lambda] Flow
  deriving (Show, Eq)

newtype Expr
  = ExpDecl Decl
  -- | ExpCall Call
  deriving (Show, Eq)

-- | Each binding carries the 'Ty' of its result so the printer can annotate it
-- and pin the representation. 'DeclConvOp' is the exception: the conversion
-- already names its target type, so it needs no separate annotation.
data Decl
  = DeclBinOp String Ty BinOp
  | DeclCall String Ty Call
  | DeclIcmp String Ty Icmp
  | DeclSelect String Ty Select
  | DeclConvOp String ConvOp
  -- | An LLVM @freeze@: a typed alias binding @name = value@ (freeze is the
  -- identity in the pure subset). The 'Ty' is the result type annotation.
  | DeclFreeze String Ty Value
  deriving (Show, Eq)

-- | A conversion: the LLVM op name (@trunc@\/@zext@\/@sext@\/@sitofp@\/…), the
-- source and target 'Ty' (conversions may cross sorts, so widths alone are
-- insufficient), and the value being converted.
data ConvOp
  = ConvOp String Ty Ty Value
  deriving (Show, Eq)

data Select
  = Select Value Value Value
  deriving (Show, Eq)

-- | A comparison: the predicate, the /operand/ 'Ty' (so the printer can pick
-- NaN-faithful codegen for floating @fcmp@ vs a plain operator for integer
-- @icmp@), and the two operands. The result is always an i1.
data Icmp
  = Icmp String Ty Value Value
  deriving (Show, Eq)

data Flow
  = FlowCall Call
  | FlowCond IfThenElse
  deriving (Show, Eq)

data BinOp
  = BinOp String Value Value
  deriving (Show, Eq)

data Call
  = Call Value [Value]
  deriving (Show, Eq)

data IfThenElse
  = IfThenElse Value Call Call
  deriving (Show, Eq)

data Value
  = Const Integer
  -- | A floating constant: the (Haskell-renderable) literal text and its 'Ty'
  -- (so the printer emits @Float@ vs @Double@ and an explicitly-typed literal).
  | FConst String Ty
  -- | A boolean constant, from an @i1@ literal (LLVM @true@\/@false@). Printed
  -- bare as @True@\/@False@.
  | BConst Bool
  | Name String
  | Unit
  deriving (Show, Eq)
